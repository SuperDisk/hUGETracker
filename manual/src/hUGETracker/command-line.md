# Command Line Options

hUGETracker supports loading a module passed as the first command line argument. Additionally, data directories can be configured using the `--conf_dir`, `--cache_dir` and `--runtime_dir` options. This allows running hUGETracker from a read-only directory, or packaging hUGETracker for Linux distributions.

| Option          | Information                                                                                                       |
|-----------------|-------------------------------------------------------------------------------------------------------------------|
| `--conf_dir`    | Where user configuration is stored (settings, keymaps, color schemes).                                            |
| `--cache_dir`   | Where intermediate files generated during song assembly are stored.                                               |
| `--runtime_dir` | Where necessary files that normally ship with hUGETracker are stored. This includes `hUGEDriver`, `halt.gb`, etc. |
