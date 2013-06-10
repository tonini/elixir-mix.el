# elixir-mix.el

Integration of Elixir's building and deployment tool: mix into Emacs.

## Installation

```lisp
(add-to-list 'load-path "~/path/to/elixir-mix.el/")
(require 'elixir-mix)
(global-elixir-mix-mode)
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
        <td><code>elixir-mix-compile</code></td>
        <td>Compile the whole Elixir application.</td>
    </tr>
    <tr>
        <td><code>elixir-mix-run</code></td>
        <td>Runs the given expression in the Elixir application context.</td>
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
(setq elixir-mix-buffer-name "*MIX*")
```

## Thanks

Special thanks to the following emacs open source projects, which I
learned a lot of emacs lisp from. :-)

* [cabbage](https://github.com/senny/cabbage)
* [ruby-compilation.el](http://www.emacswiki.org/emacs/ruby-compilation.el)
* [yasnippet](https://github.com/capitaomorte/yasnippet)
* [EmacsWiki](http://www.emacswiki.org/emacs/)

## Contributions are very welcome!

1. Fork elixir-mix.el
2. Create a topic branch - `git checkout -b my_branch`
4. Push to your branch - `git push origin my_branch`
5. Send me a pull-request for your topic branch
6. That's it!
