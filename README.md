# Mox

Mox is a Discord bot to help with playing RPGs in the Moxie system.

To see all your options, use the help command:

```
/help
```

A few commands are detailed below, but always use `/help` for the most up to date options.

## `/roll`
The roll command lets you roll a pool of dice with thorns, and displays your final result (after any cuts).

To roll dice, give Mox an expression like `2d` (two d6s) or `3d2t` (three d6s and two thorns).

There are also a few more options for variations on normal rolls. Use `/help roll` to see information on those.

## `/pool`
The `pool` command lets you create and manage persistent diminishing pools.

### Quickstart
You can start a persistent pool like this:

```
/pool new 6 Ritual
```

and then roll it like:

```
/pool roll Ritual
```

Pools will be stored between sessions, but will be deleted if they haven't been rolled in a while.

Most pool operations take an optional `scope` parameter - by default, pools are stored at the channel level, and are accessible to anyone in that channel. They can also be stored at the server level (and will be accessible to everyone in that server), e.g.

```
/pool new 8 Dragon server
```

If you want a pool to stay private to only you, you can use the `pool` commands in a DM with Mox.

### Subcommands
#### `new`
Creates a pool.

```
/pool new {num_dice} {pool_name} [{scope}]
```

#### `roll`
Rolls a pool.
```
/pool roll {pool_name} [{scope}]
```

#### `delete`
Deletes a pool.
```
/pool delete {pool_name} [{scope}]
```

#### `set`
Manually updates the number of dice in a pool.
```
/pool set {pool_name} {num_dice} [{scope}]
```
The `num_dice` argument supports absolute and relative values. For instance, to change the pool to exactly six dice, use `/pool set {pool_name} 6`. To add a die back into the pool, use `/pool set {pool_name} +1`. To remove three dice from the pool, use `/pool set {pool_name} -3`.

#### `reset`
Sets the pool back to how many dice it had when created.
```
/pool reset {pool_name} [{scope}]
```

#### `check`
Checks how many dice are currently in the pool without rolling it.
```
/pool check {pool_name} [{scope}]
```


### Contribution
Contributions are always welcome, no matter your skill level! Check out the Oddity Press Discord for discussion of this bot and other community tools and help getting started.

Feature requests are also very appreciated. Open an issue on this repository or bring up your suggestion on Discord! It's super helpful if you include information about why a particular feature is useful to you and how you're thinking it would work from a user's point of view.

#### Past problems

```
Error in user data setup: Invalid Form Body (1.options.0.options.1.description: Must be between 1 and 100 in length.)
```

This error will show up as soon as the bot starts, preventing it from responding to anything. The total length of any slash command description must be less than 100 characters.
