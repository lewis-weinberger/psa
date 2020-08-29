# psa

> a simple scheduled alerts program using the Discord WEB API

[![Linux](https://github.com/lewis-weinberger/psa/workflows/Linux/badge.svg)](https://github.com/lewis-weinberger/psa/actions)
[![MacOS](https://github.com/lewis-weinberger/psa/workflows/MacOS/badge.svg)](https://github.com/lewis-weinberger/psa/actions)
[![Windows](https://github.com/lewis-weinberger/psa/workflows/Windows/badge.svg)](https://github.com/lewis-weinberger/psa/actions)

## Usage

Install [SBCL](http://www.sbcl.org/platform-table.html) and [Quicklisp](https://www.quicklisp.org/beta/).

Clone the repository to your `quicklisp/local-projects` directory:

```
git clone https://github.com/lewis-weinberger/psa.git ~/quicklisp/local-projects/psa
```

Compile the program:

```sh
sbcl --eval "(ql:quickload :psa)" --eval "(asdf:make :psa)"
```

This should create a `bin` directory containing the executable `psa`. Quicklisp should bring in the dependencies ([drakma](https://edicl.github.io/drakma/), [cl-json](https://common-lisp.net/project/cl-json/cl-json.html), [local-time](https://common-lisp.net/project/local-time/) and [swank](https://www.cliki.net/Swank)). Finally you can find usage help by invoking `psa` with no arguments, which should print something like:

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

## Event format

An event file should be written in JSON with the following fields:

```json
{
    "date": "2020-07-29",
    "description": "Saskia's Birthday",
    "style": "Public announcement"
}
```
Note the date format should be "YYYY-MM-DD". For every event you'd like to be notified about, add a JSON file as above to your desired events directory. `psa` will scan this directory, parse the event files and send a message to the given Discord webhook when that event is drawing near. The message will appear as:

```
[2020-07-29] Public announcement: Saskia's Birthday
```