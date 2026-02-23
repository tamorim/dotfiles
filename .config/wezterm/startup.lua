local wezterm = require('wezterm')
local mux = wezterm.mux
local home_dir = wezterm.home_dir

return function()
  -- local app_tab, app_pane, window = mux.spawn_window({ cwd = home_dir .. '/projects/<project>' })
  -- window:gui_window():maximize()
  --
  -- app_tab:set_title('web-apps')
  -- app_pane:split({ direction = 'Right', size = 0.3 })
  --
  -- app_pane:activate()
end
