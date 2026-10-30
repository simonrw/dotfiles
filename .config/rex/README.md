# Rex

Lua keybindings live in `init.lua`. macOS UI and terminal preferences live in `preferences.json`. Python 3 and macOS `defaults` are required.

## Sync preferences

After changing settings in Rex, quit the app so its preferences are flushed, then export:

```sh
mise run rex:export
git diff -- .config/rex/preferences.json
```

Commit the manifest and pull it on another Mac. With Rex quit there, apply it:

```sh
mise run rex:apply
```

Reopen Rex afterwards. Neither command runs automatically during dotfiles installation. Install the JetBrainsMonoNL Nerd Font Mono font separately.

## Manifest

The manifest is also the allowlist. Export refreshes only its existing keys; it never discovers or adds other preferences. `preferences` holds strings, booleans, integers, floats, and string arrays. `json_data` holds readable JSON that Rex stores as plist Data, including the custom themes and their IDs.

To track another setting, add its exact key and a value of the correct type to the appropriate section, then export. Inspect available keys with `defaults read com.superlogical.rex`. Font size was not stored in this domain when the manifest was created, so no value is invented.

If a tracked key is missing, export fails without changing the manifest. Set it in Rex or remove it from the manifest before exporting again. Removing a key stops managing it; apply does not delete its saved value.

Apply validates the entire manifest before writing individual keys and preserves unrelated preferences. It replaces each tracked array, including the custom-theme collection, rather than merging it. Runtime failures during apply can leave earlier keys applied; fix the error and rerun.

Announcements, receipts, last focused session, window geometry, onboarding/update history, sync hosts, and server/network settings are intentionally excluded. Do not symlink or import the entire preferences plist: it mixes configuration with runtime state.

For testing against a disposable domain or another manifest:

```sh
python3 .config/rex/preferences.py export --domain com.example.test --manifest /tmp/preferences.json
python3 tests/rex-preferences.py
```
