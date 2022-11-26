# Vega And Altair

Vega and Altair are the starc-crossed lovers of the Milky Way

This server is a dating site.

# The Laika Server

Vega and Altair use the [Laika Gemini Server](https://github.com/gordonguthrie/laika.git)

There is a simpler [example](https://github.com/gordonguthrie/laika.git) of using Laika

# How to play with Vega And Altair

This method assumes you have [Docker](https://www.docker.com/get-started/) installed on your desktop. And doesn't require Erlang to be installed on your machine.

In one terminal:

```
git clone git@github.com:gordonguthrie/vega_and_altair.git
cd vega_and_altair/erlang
./generate_self_signed_certs.sh
cd ../
docker-compose up
```

You now have Vega And Altair Example running in a terminal, open another terminal:

```
cd vega_and_altair/docker/scripts
./start_vega_and_altair
```

This will bind that terminal to the running docker instance and log you in.

The directory with the erlang code on your machine is mounted into the container.

Any changes you make on your host will be reflected in the container.

In that terminal:

```
cd /vega_and_altair
rebar3 shell
```

You now have `vega_and_altair` running in a shell

Use your favourite Gemini client to attach to `gemini://localhost` and away you go:
* on Android in the App store [Deedum](https://play.google.com/store/apps/details?id=ca.snoe.deedum&hl=en_GB&gl=US&pli=1)
* on Mac, Windows, Linux, iOS (testflight), Android (beta) [Lagrange](https://gmi.skyjake.fi/lagrange/)

# In production

In production you will need to rejig the certificates as the SSL connection is signed for the URL so gently frig `generate_self_signed_certs.sh`.

At a minimum replace `localhost` with the URL you are deploying to, but it would be polite to replace Deneb as well.
