use super::*;
use crate::client::Context;
use crate::model::channel::Message;
use aho_corasick::{AhoCorasickBuilder, MatchKind};
use uwl::{StrExt, UnicodeStream};

#[derive(Debug, Clone, PartialEq)]
pub enum Prefix<'a> {
    Punct(&'a str),
    Mention(&'a str),
    None,
    #[doc(hidden)]
    __Nonexhaustive,
}

pub fn parse_prefix<'a>(
    ctx: &mut Context,
    msg: &'a Message,
    config: &Configuration,
) -> Vec<(Prefix<'a>, &'a str)> {
    let mut stream = UnicodeStream::new(&msg.content);
    stream.take_while(|s| s.is_whitespace());

    if let Some(ref mention) = config.on_mention {
        if let Ok(id) = stream.parse("<@(!){}>") {
            if id.is_numeric() && mention == id {
                stream.take_while(|s| s.is_whitespace());
                return vec![(Prefix::Mention(id), stream.rest())];
            }
        }
    }

    let text = stream.rest();

    let mut prefixes: Vec<String> = config
        .dynamic_prefixes
        .iter()
        .filter_map(|f| f(ctx, msg))
        .collect();
    prefixes.extend(config.prefixes.clone());

    let ac = AhoCorasickBuilder::new()
        .auto_configure(&prefixes)
        .match_kind(MatchKind::LeftmostFirst)
        .build(&prefixes);
    let mut iter = ac.find_iter(text);

    let mut actual = vec![];

    stream = UnicodeStream::new(text);

    while let Some(mat) = iter.next() {
        let prefix = &text[mat.start()..mat.end()];

        // Got'em!
        // We don't care about the start of the actual match
        // The end of the match will be the start of the text
        let start = mat.end();

        // Gotta advance the stream
        stream.set(mat.end());

        if config.with_whitespace.prefixes {
            stream.take_while(|s| s.is_whitespace());
        }

        // Keep looking for an end (non-exclusive)
        let mut end = None;

        while let Some(mat) = iter.next() {
            // Gotta advance the stream
            stream.set(mat.end());

            if config.with_whitespace.prefixes {
                stream.take_while(|s| s.is_whitespace());
            }

            if stream.eat("end") {
                // That's all, folks
                end = Some(mat.start());
                break;
            }
        }

        actual.push((Prefix::Punct(prefix), match end {
            Some(end) => text[start..end].trim_end(),
            None => text[start..].trim_end()
        }));
    }

    if actual.is_empty() {
        actual.push((Prefix::None, &msg.content))
    }

    actual
}

struct CommandParser<'msg, 'groups, 'config, 'ctx> {
    msg: &'msg Message,
    stream: UnicodeStream<'msg>,
    groups: &'groups [&'static CommandGroup],
    config: &'config Configuration,
    ctx: &'ctx Context,
    unrecognised: Option<&'msg str>,
}

impl<'msg, 'groups, 'config, 'ctx> CommandParser<'msg, 'groups, 'config, 'ctx> {
    fn new(
        msg: &'msg Message,
        stream: UnicodeStream<'msg>,
        groups: &'groups [&'static CommandGroup],
        config: &'config Configuration,
        ctx: &'ctx Context,
    ) -> Self {
        CommandParser {
            msg,
            stream,
            groups,
            config,
            ctx,
            unrecognised: None,
        }
    }

    fn next_text(&self, length: impl FnOnce() -> usize) -> &'msg str {
        if self.config.by_space {
            self.stream.peek_until(|s| s.is_whitespace())
        } else {
            self.stream.peek_for(length())
        }
    }

    fn as_lowercase(&self, s: &str, f: impl FnOnce(&str) -> bool) -> bool {
        if self.config.case_insensitive {
            let s = s.to_lowercase();
            f(&s)
        } else {
            f(s)
        }
    }

    fn check_discrepancy(&self, options: &impl CommonOptions) -> Result<(), DispatchError> {
        if options.owners_only() && !self.config.owners.contains(&self.msg.author.id) {
            return Err(DispatchError::OnlyForOwners);
        }

        if options.only_in() == OnlyIn::Dm && !self.msg.is_private() {
            return Err(DispatchError::OnlyForDM);
        }

        if (!self.config.allow_dm || options.only_in() == OnlyIn::Guild) && self.msg.is_private() {
            return Err(DispatchError::OnlyForGuilds);
        }

        #[cfg(feature = "cache")]
        {
            if let Some(guild_id) = self.msg.guild_id {
                let guild = match guild_id.to_guild_cached(&self.ctx) {
                    Some(g) => g,
                    None => return Ok(()),
                };

                let guild = guild.read();

                let perms = guild.permissions_in(self.msg.channel_id, self.msg.author.id);

                if !perms.contains(*options.required_permissions()) &&
                    !(options.owner_privilege() &&
                        self.config.owners.contains(&self.msg.author.id)) {
                    return Err(DispatchError::LackingPermissions(*options.required_permissions()));
                }

                if let Some(member) = guild.members.get(&self.msg.author.id) {
                    if !perms.administrator() && !has_correct_roles(options, &guild, &member) {
                        return Err(DispatchError::LackingRole);
                    }
                }

            }
        }

        Ok(())
    }

    fn command(&mut self, command: &'static Command) -> Result<Option<&'static Command>, DispatchError> {
        for name in command.options.names {
            // FIXME: If `by_space` option is set true, we shouldn't be retrieving the block of text
            // again and again for the command name.
            let n = self.next_text(|| name.chars().count());

            let equals = self.as_lowercase(n, |n| n == *name && !self.config.disabled_commands.contains(n));

            if equals {
                self.stream.increment(n.len());

                if self.config.with_whitespace.commands {
                    self.stream.take_while(|s| s.is_whitespace());
                }

                for sub in command.options.sub_commands {
                    if let Some(cmd) = self.command(sub)? {
                        self.unrecognised = None;
                        return Ok(Some(cmd));
                    }
                }

                self.check_discrepancy(&command.options)?;

                self.unrecognised = None;
                return Ok(Some(command));
            }

            self.unrecognised = Some(n);
        }

        Ok(None)
    }

    fn group(&mut self, group: &'static CommandGroup) -> Result<(Option<&'msg str>, &'static CommandGroup), DispatchError> {
        // Do not bother going through this group's prefixes if it doesn't contain any, but
        // try if its sub-groups maybe do and are satisfied in the message.
        if group.options.prefixes.is_empty() {
            for sub_group in group.sub_groups {
                let x = self.group(*sub_group)?;

                if x.0.is_some() {
                    return Ok(x);
                }
            }

            return Ok((None, group));
        }

        for p in group.options.prefixes {
            let pp = self.next_text(|| p.chars().count());

            if *p == pp {
                self.stream.increment(pp.len());

                if self.config.with_whitespace.groups {
                    self.stream.take_while(|s| s.is_whitespace());
                }

                for sub_group in group.sub_groups {
                    let x = self.group(*sub_group)?;

                    if x.0.is_some() {
                        return Ok(x);
                    }
                }

                self.check_discrepancy(&group.options)?;
                return Ok((Some(pp), group));
            }
        }

        Ok((None, group))
    }

    fn parse(mut self, prefix: Prefix<'msg>) -> Result<Invoke<'msg>, Result<Option<&'msg str>, DispatchError>> {
        let pos = self.stream.offset();
        for group in self.groups {
            let (gprefix, group) = match self.group(*group) {
                Ok(t) => t,
                Err(err) => return Err(Err(err)),
            };

            if gprefix.is_none() && !group.options.prefixes.is_empty() {
                unsafe { self.stream.set_unchecked(pos) };
                continue;
            }

            for command in group.commands {
                let command = match self.command(command)  {
                    Ok(c) => c,
                    Err(err) => return Err(Err(err)),
                };

                if let Some(command) = command {
                    if group.options.prefixes.is_empty() {
                        // Might seem a little late to do at this point of time, like it would
                        // have been better to test against any discrepancies of this group
                        // in the `group` method then continue said test in the `command` method,
                        // but we can't accurately perform that on a group with no prefixes.
                        // I.e. Does the message contain this group? Well, maybe God knows, but we
                        // certainly don't and cannot if we don't have any sort of text to verify with.
                        if let Err(err) = self.check_discrepancy(&group.options) {
                            return Err(Err(err));
                        }
                    }

                    return Ok(Invoke::Command {
                        prefix,
                        group,
                        gprefix,
                        command,
                        args: self.stream.rest(),
                    });
                }
            }

            // Only execute the default command if a group prefix is present.
            if let Some(command) = group.options.default_command {
                if gprefix.is_some() {
                    return Ok(Invoke::Command {
                        prefix,
                        group,
                        gprefix,
                        command,
                        args: self.stream.rest(),
                    });
                }
            }

            unsafe { self.stream.set_unchecked(pos) };
        }

        Err(Ok(self.unrecognised))
    }
}

pub(crate) fn parse_command<'a>(
    message: &'a str,
    msg: &'a Message,
    prefix: Prefix<'a>,
    groups: &[&'static CommandGroup],
    config: &Configuration,
    ctx: &Context,
    help_was_set: Option<&[&'static str]>,
) -> Result<Invoke<'a>, Result<Option<&'a str>, DispatchError>> {
    let mut stream = UnicodeStream::new(message);
    stream.take_while(|s| s.is_whitespace());

    // We take precedence over commands named help command's name.
    if let Some(names) = help_was_set {
        for name in names {
            let present = if config.case_insensitive {
                let n = stream.peek_for(name.chars().count());

                if name == &n.to_lowercase() {
                    stream.increment(n.len());

                    true
                } else {
                    false
                }
            } else {
                stream.eat(name)
            };

            if present {
                stream.take_while(|s| s.is_whitespace());

                let args = stream.rest();

                return Ok(Invoke::Help { prefix, name, args });
            }
        }
    }

    CommandParser::new(msg, stream, groups, config, ctx).parse(prefix)
}

#[derive(Debug)]
pub enum Invoke<'a> {
    Command {
        prefix: Prefix<'a>,
        // Group prefix
        gprefix: Option<&'a str>,

        group: &'static CommandGroup,
        command: &'static Command,
        args: &'a str,
    },
    Help {
        prefix: Prefix<'a>,
        name: &'static str,
        args: &'a str,
    },
}
