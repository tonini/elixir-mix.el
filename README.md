# elixir-mix.el

[![Build Status](https://travis-ci.org/tonini/elixir-mix.el.png?branch=introduce-testing)](https://travis-ci.org/tonini/elixir-mix.el)

Integration of Elixir's building and deployment tool: mix into Emacs.

## Installation

### ELPA

elixir-mix.el is available on both community maintained repositories -
[Marmalade](http://marmalade-repo.org/) and
[MELPA](http://melpa.milkbox.net/). Just run `M-x package-install
[RET] elixir-mix [RET]`
inside your emacs and you're ready to go.

If you're not already using ELPA, check the [emacswiki](http://www.emacswiki.org/emacs/ELPA) page to get
familiar with it.

### Manual

```lisp
(add-to-list 'load-path "~/path/to/elixir-mix.el/")
(require 'elixir-mix)
(global-elixir-mix-mode) ;; enable elixir-mix
```

## Usage

<table>
    <tr>
        <th>Command (For the <code>M-x</code> prompt.)</th>
        <th>Description</th>
    </tr>
    <tr>
        <td><code>global-elixir-mix-mode</code></td>
        <td>enable or disable elixir-mix.el</td>
    </tr>
    <tr>
        <td><code>elixir-mix-new</code></td>
        <td>Create a new Elixir application.</td>
    </tr>
     <tr>
        <td><code>elixir-mix-test</code></td>
        <td>Run the whole Elixir application test suite.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-test-this-buffer</code></td>
        <td>Run the current buffer through <code>mix test</code> command.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-test-file</code></td>
        <td>Run a file through <code>mix test</code> command.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-test-at-point</code></td>
        <td>Run the test at point.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-compile</code></td>
        <td>Compile the whole Elixir application.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-run</code></td>
        <td>Runs the given expression in the Elixir application context.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-deps-with-prompt</code></td>
        <td>Prompt for <code>mix deps</code> commands.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-local-with-prompt</code></td>
        <td>Prompt for <code>mix local</code> commands.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-local-install</code></td>
        <td>Prompt for <code>mix local.install</code> PATH or URL.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-local-install-with-path</code></td>
        <td>Runs <code>mix local.install</code> and prompt for a PATH as argument.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-local-install-with-url</code></td>
        <td>Runs <code>mix local.install</code> and prompt for a URL as argument.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-help</code></td>
        <td>Show help output for a specific mix command.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-execute</code></td>
        <td>Run any command in the context of the application.</td>
    </tr>
</table>

## Configuration

`elixir-mix-command` Path to the executable `mix` shell command.

```lisp
(setq elixir-mix-command "/usr/local/bin/mix")
```

`elixir-mix-buffer-name` Name of the buffer used for mix shell output.

```lisp
(setq elixir-mix-buffer-name "*mix*")
```

## Contributions are very welcome!

1. Fork elixir-mix.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!

[![githalytics.com alpha](https://cruel-carlota.pagodabox.com/b5d8e53f0a7f0bf174bab801201a53f7 "githalytics.com")](http://githalytics.com/tonini/elixir-mix.el)
