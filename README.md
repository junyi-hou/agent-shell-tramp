# agent-shell-tramp

Generic TRAMP support for [`agent-shell`](https://github.com/xenodium/agent-shell).

`agent-shell-tramp` keeps TRAMP-specific behavior outside `agent-shell` core. It lets ACP agents run from remote TRAMP buffers by relying on Emacs file handlers for process startup, translating paths between TRAMP and remote-local forms, and storing remote session transcripts locally.

## Requirements

- Emacs 29.1+
- `agent-shell`
- A recent `acp.el` with TRAMP/file-handler process support. This landed upstream in [xenodium/acp.el#20](https://github.com/xenodium/acp.el/pull/20).
- A working TRAMP backend. The file-handler approach is generic TRAMP, but each backend must support long-lived remote processes.

## Installation

Clone this repository and add it to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/agent-shell-tramp")
(require 'agent-shell-tramp)
(agent-shell-tramp-mode 1)
```

With `use-package` and straight.el:

```elisp
(use-package agent-shell-tramp
  :straight (:host github :repo "junyi-hou/agent-shell-tramp")
  :after agent-shell
  :config
  (agent-shell-tramp-mode 1))
```

## Usage

Enable the global minor mode:

```elisp
(agent-shell-tramp-mode 1)
```

When `agent-shell` is started from a remote TRAMP buffer, upstream `acp.el` starts the ACP client through Emacs file handlers. That lets TRAMP own the transport instead of this package constructing an explicit `ssh ... shell -lc ...` wrapper.

When `agent-shell-cwd` is remote:

- TRAMP paths such as `/ssh:host:/project/file.el` are sent to the agent as remote-local paths like `/project/file.el`.
- Remote-local absolute paths from the agent are resolved back into TRAMP paths for Emacs file handlers.
- Remote transcripts are stored locally under `agent-shell-tramp-transcript-directory`.

Relative paths are left unchanged.

## Notes

This package no longer wraps process startup in SSH directly. Older versions used `agent-shell-tramp-remote-shell` to build an `ssh ... shell -lc ...` command; that variable is obsolete because process startup is now delegated to `acp.el` and TRAMP file handlers.

This package is based on earlier work in [csheaff/agent-shell-tramp-rpc](https://github.com/csheaff/agent-shell-tramp-rpc) and the original discussion in [xenodium/agent-shell#205](https://github.com/xenodium/agent-shell/pull/205).

## License

GPL-3.0
