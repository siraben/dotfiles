import datetime
from kitty.fast_data_types import Screen, add_timer, get_options
from kitty.tab_bar import (
    DrawData,
    ExtraData,
    TabBarData,
    as_rgb,
    draw_tab_with_powerline,
)

timer_id = None

def draw_tab(
    draw_data: DrawData,
    screen: Screen,
    tab: TabBarData,
    before: int,
    max_title_length: int,
    index: int,
    is_last: bool,
    extra_data: ExtraData,
) -> int:
    global timer_id
    if timer_id is None:
        timer_id = add_timer(_redraw_tab_bar, 1.0, True)

    # Draw the tab with powerline style
    end = draw_tab_with_powerline(
        draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
    )

    # Draw clock on the right side if this is the last tab
    if is_last:
        date_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        date_time_len = len(date_time) + 2  # Add padding

        # Always position the clock at the right edge
        clock_x = screen.columns - date_time_len

        # Only draw the clock if there's enough space
        if clock_x > end:
            # Fill space between tab and clock
            screen.cursor.x = end
            screen.draw(" " * (clock_x - end))

            # Draw the date/time
            opts = get_options()
            default_bg = as_rgb(int(draw_data.default_bg))
            screen.cursor.fg = 0
            screen.cursor.bg = default_bg
            screen.cursor.x = clock_x
            screen.draw(f" {date_time}")

    return screen.cursor.x

def _redraw_tab_bar(timer_id):
    for tm in _get_tm():
        tm.mark_tab_bar_dirty()

def _get_tm():
    from kitty.fast_data_types import get_boss
    boss = get_boss()
    for window in boss.all_windows:
        tabman = window.tabref()
        if tabman is not None:
            yield tabman
