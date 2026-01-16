-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices.

-- For example, changing the initial geometry for new windows:
config.initial_cols = 120
config.initial_rows = 28

-- or, changing the font size and color scheme.
config.font_size = 14
config.color_scheme = 'AdventureTime'

config.keys = {
  {
    key = 'd',
    mods = 'CMD',
    action = wezterm.action.SplitHorizontal { domain = 'CurrentPaneDomain' },
    -- CTRL+SHIFT+ALT 	" 	SplitVertical={domain="CurrentPaneDomain"}
-- CTRL+SHIFT+ALT 	% 	SplitHorizontal={domain="CurrentPaneDomain"}
  },

  {
    key = 'd',
    mods = 'CMD|SHIFT',
    -- action = wezterm.action.SplitVertical
    action = wezterm.action.SplitVertical { domain = 'CurrentPaneDomain' },
  }
}

-- Finally, return the configuration to wezterm:
return config
