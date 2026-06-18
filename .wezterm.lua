-- Pull in the wezterm API
local wezterm = require 'wezterm' ---@type Wezterm
local act = wezterm.action

-- This will hold the configuration.
local config = wezterm.config_builder() ---@type Config

-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
config.color_scheme = 'Tokyo Night'
-- config.color_scheme = 'AdventureTime'

config.window_decorations = "INTEGRATED_BUTTONS|RESIZE"

local gui_font = wezterm.font {
  family = 'Roboto',
  weight = 'Medium',
  stretch = "ExtraExpanded"
}
config.window_frame = {
  font = gui_font,
  font_size = 16.0,
}

config.command_palette_font = gui_font
config.command_palette_font_size = 16.0

-- Disable, since wezterm is broken and incorrectly implements this kitty stuff
-- See https://github.com/wezterm/wezterm/pull/7312
-- distinguish between enter and shift+enter
-- config.enable_kitty_keyboard = true
config.ssh_domains = {
  {
    name = 'nixos',
    remote_address = 'nixos',
    multiplexing = 'None',
    username = 'omad',

    -- When multiplexing == "None", default_prog can be used
    -- to specify the default program to run in new tabs/panes.
    -- Due to the way that ssh works, you cannot specify default_cwd,
    -- but you could instead change your default_prog to put you
    -- in a specific directory.
    default_prog = { 'fish' },

    -- assume that we can use syntax like:
    -- "env -C /some/where $SHELL"
    -- using whatever the default command shell is on this
    -- remote host, so that shell integration will respect
    -- the current directory on the remote host.
    assume_shell = 'Posix',
  },
}
function docker_list()
  local docker_list = {}
  local success, stdout, stderr = wezterm.run_child_process {
    '/usr/local/bin/docker',
    'container',
    'ls',
    '--format',
    '{{.ID}}:{{.Names}}',
  }
  for _, line in ipairs(wezterm.split_by_newlines(stdout)) do
    local id, name = line:match '(.-):(.+)'
    if id and name then
      docker_list[id] = name
    end
  end
  return docker_list
end

function make_docker_label_func(id)
  return function(name)
    local success, stdout, stderr = wezterm.run_child_process {
      '/usr/local/bin/docker',
      'inspect',
      '--format',
      '{{.State.Running}}',
      id,
    }
    local running = stdout == 'true\n'
    local color = running and 'Green' or 'Red'
    return wezterm.format {
      { Foreground = { AnsiColor = color } },
      { Text = 'docker container named ' .. name },
    }
  end
end

function make_docker_fixup_func(id)
  return function(cmd)
    cmd.args = cmd.args or { '/bin/sh' }
    local wrapped = {
      '/usr/local/bin/docker',
      'exec',
      '-it',
      id,
    }
    for _, arg in ipairs(cmd.args) do
      table.insert(wrapped, arg)
    end

    cmd.args = wrapped
    return cmd
  end
end

function compute_exec_domains()
  local exec_domains = {}
  for id, name in pairs(docker_list()) do
    table.insert(
      exec_domains,
      wezterm.exec_domain(
        'docker:' .. name,
        make_docker_fixup_func(id),
        make_docker_label_func(id)
      )
    )
  end
  return exec_domains
end

config.exec_domains = compute_exec_domains()

local jterm_remotes = {
  adias = 'JupyterHub:Adias',
  csiro = 'JupyterHub:CSIRO'
}
for name, label in pairs(jterm_remotes) do
  table.insert(config.exec_domains, wezterm.exec_domain(label, function(cmd)
      -- The "cmd" parameter is a SpawnCommand object.
      -- You can log it to see what's inside:
      wezterm.log_info('launching jterm ' + name, cmd)
      cmd.args = {
        '/Users/aye011/.local/bin/jterm', 'connect', name, '--shell', 'fish'
      }
      if cmd.cwd then
        table.insert(cmd.args, '--cwd')
        table.insert(cmd.args, cmd.cwd)
      end
      return cmd
    end
  , label
    )
  )
end

config.launch_menu = {
  {
    args = {"htop"}
  }
}

config.keys = {
  -- Show the Launcher
  { key = 'l', mods = 'ALT', action = act.ShowLauncher },
  -- Make Shift + Enter actually work!
  { key = "Enter", mods = "SHIFT", action = act { SendString = "\x1b\r" } },
  { key = "P",   mods = "CMD", action = act.ActivateCommandPalette },
  {
    key = 'd',
    mods = 'CMD',
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' },
    -- CTRL+SHIFT+ALT 	" 	SplitVertical={domain="CurrentPaneDomain"}
    -- CTRL+SHIFT+ALT 	% 	SplitHorizontal={domain="CurrentPaneDomain"}
  },
  { key = 't', mods = 'SUPER|SHIFT', action = act.SpawnTab 'DefaultDomain' },
  {
    key = 'd',
    mods = 'CMD|SHIFT',
    -- action = act.SplitVertical
    action = act.SplitVertical { domain = 'CurrentPaneDomain' },
  },
  {
    key = 'Insert',
    mods = 'SHIFT',
    action = act.PasteFrom 'Clipboard'
  }
}

-- Finally, return the configuration to wezterm:
return config
