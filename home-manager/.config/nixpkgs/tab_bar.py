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

    end = draw_tab_with_powerline(
        draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
    )

    # Draw clock on the right side if this is the last tab
    if is_last:
        date_time = datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")
        clock_text = f" {date_time} "

        opts = get_options()
        bar_bg_color = opts.tab_bar_background if opts.tab_bar_background is not None else draw_data.default_bg
        bar_bg = as_rgb(int(bar_bg_color))
        bar_fg = as_rgb(int(draw_data.inactive_fg))

        clock_x = screen.columns - len(clock_text)

        if clock_x > end:
            # Fill space between tab and clock with the bar background
            screen.cursor.fg = bar_fg
            screen.cursor.bg = bar_bg
            screen.cursor.x = end
            screen.draw(" " * (clock_x - end))

            # Draw the date/time
            screen.cursor.x = clock_x
            screen.draw(clock_text)

    return end

def _redraw_tab_bar(timer_id):
    from kitty.fast_data_types import get_boss
    tm = get_boss().active_tab_manager
    if tm is not None:
        tm.mark_tab_bar_dirty()
