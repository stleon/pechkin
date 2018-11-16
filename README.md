# pechkin

An OTP application for sending messages in telegram channels.

## Configuration

### Number of workers

You can define max telegram workers in **sys.config**.
By default it number of yours machine CPUs:

```
{pechkin, [
    {max_workers, "${MAX_WORKERS}"}
]}
```

Workers are taken from the pool by **round-robin**.

## Install

In future **pechkin** will be distributed, but now you can include him in release.

### rebar.config

Add dependency in **deps** section of yours **rebar.config**

```
{deps, [
        {pechkin,  {git, "git://github.com/stleon/pechkin.git", {tag, "0.0.1"}}}
       ]
}.
```

And in **relx** section add **pechkin** to list of applications that be included in the release:

```
{relx, [{release, { release_name, "vsn" },
         [sasl,
          pechkin
          ...]},
```

Then in **app.src** add **pechkin**:
```
  {applications,
   [kernel,
    stdlib,
    pechkin
   ]},
```

## API

### send(Message)

Here is some workflow (if you run from **shell**) :

```
1> rr("include/pechkin.hrl").
2> ApiKey = <<"bot_api_key">>.
3> Text = <<"Hello, world!">>.
4> Channel = <<"@channel_name">>. %% or id
5> Message = #telegram_message{apikey = ApiKey, channel = Channel, text = Text}.
6> pechkin:send(Message).
```

## Roadmap
- http API
- balancer (if there are many **pechkin**s in the cluster)