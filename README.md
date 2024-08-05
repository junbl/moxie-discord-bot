# Mox

Mox is a Discord bot to help with playing RPGs in the Moxie system.

Its main function is to interact with diminishing pools. You can roll a pool like this:

```
/quickpool 6
```

This will roll six dice and show you the results. If you want Mox to keep track of your pool for multiple rolls, you'll want the `/pool` command.

## `/pool`

### Quickstart
You can start a persistent pool like this:

```
/pool 6 Ritual
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

If you want a pool to stay private to only you, you can use the pool commands in a DM with Mox.

### `/pool` command reference
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
