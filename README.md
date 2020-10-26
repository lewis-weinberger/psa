# psa

> a simple scheduled alerts program using the Discord WEB API

[![Linux](https://github.com/lewis-weinberger/psa/workflows/Linux/badge.svg)](https://github.com/lewis-weinberger/psa/actions)
[![MacOS](https://github.com/lewis-weinberger/psa/workflows/MacOS/badge.svg)](https://github.com/lewis-weinberger/psa/actions)
[![Windows](https://github.com/lewis-weinberger/psa/workflows/Windows/badge.svg)](https://github.com/lewis-weinberger/psa/actions)

## Usage

Install a Common Lisp implementation (e.g. [SBCL](http://www.sbcl.org/platform-table.html), [ECL](https://common-lisp.net/project/ecl/), etc.) and [Quicklisp](https://www.quicklisp.org/beta/).

Clone the repository to your `quicklisp/local-projects` directory:

```
git clone https://github.com/lewis-weinberger/psa.git ~/quicklisp/local-projects/psa
```

Compile the program using `make`:

```sh
make LISP=sbcl    # set LISP to desired implementation
```

This should create the executable `psa`. Quicklisp should bring in the dependencies ([drakma](https://edicl.github.io/drakma/), [cl-json](https://common-lisp.net/project/cl-json/cl-json.html), and [local-time](https://common-lisp.net/project/local-time/)). Finally you can find usage help by invoking `psa` with no arguments, which should print something like:

```
_______________
psa version 0.1

Usage:
    psa [WEBHOOK-URL] [EVENTS-DIRECTORY] [TIME-FRAME]

where:
    WEBHOOK-URL -- Discord webhook URL
    EVENTS-DIRECTORY -- path to directory containing event files
    TIME-FRAME -- number of days before event to issue notifications
```

Each time `psa` is invoked it will scan the provided events directory, parse any JSON event files (see below) and send a message to the given Discord webhook when that event is drawing near. Thus a simple setup could use a job scheduler (such as [cron](https://en.wikipedia.org/wiki/Cron)) to call `psa` every day. Example [systemd](https://www.freedesktop.org/wiki/Software/systemd/) timer and service units are included in the systemd directory. Enabling the timer unit will result in the service unit running daily at 12:00. These units assume that psa is installed at `/usr/local/bin`.

## Event format

An event file should be written in JSON with the following fields:

```json
{
    "date": "2020-07-29",
    "description": "Saskia's Birthday",
    "style": "Public announcement"
}
```
Note the date format should be "YYYY-MM-DD". For every event you'd like to be notified about, add a JSON file as above to your desired events directory. The message for the above example will appear as:

```
[2020-07-29] Public announcement: Saskia's Birthday
```
