import time
from kitty.fast_data_types import (
    Screen,
    add_timer,
    get_boss,
    get_options,
    remove_timer,
)
from kitty.tab_bar import (
    DrawData,
    ExtraData,
    TabBarData,
    as_rgb,
    draw_tab_with_powerline,
)

_TIMER_ATTRIBUTE = "_siraben_tab_bar_timer_id"
_TIMER_INTERVAL = 60.0
timer_id = None


def _install_timer() -> int:
    boss = get_boss()
    previous_timer_id = getattr(boss, _TIMER_ATTRIBUTE, None)
    if previous_timer_id is not None:
        remove_timer(previous_timer_id)

    new_timer_id = add_timer(_redraw_tab_bar, _TIMER_INTERVAL, True)
    setattr(boss, _TIMER_ATTRIBUTE, new_timer_id)
    return new_timer_id


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
        timer_id = _install_timer()

    end = draw_tab_with_powerline(
        draw_data, screen, tab, before, max_title_length, index, is_last, extra_data
    )

    # Draw clock on the right side if this is the last tab
    if is_last:
        clock_text = time.strftime(" %H:%M ")

        opts = get_options()
        bar_bg_color = (
            opts.tab_bar_background
            if opts.tab_bar_background is not None
            else draw_data.default_bg
        )
        bar_bg = as_rgb(int(bar_bg_color))
        bar_fg = as_rgb(int(draw_data.inactive_fg))

        clock_x = screen.columns - len(clock_text)

        if clock_x > end:
            # The last rendered tab can leave bold/italic enabled. Do not let
            # those styles leak into the right-side status or trailing space.
            screen.cursor.bold = False
            screen.cursor.italic = False

            # Fill space between tab and clock with the bar background
            screen.cursor.fg = bar_fg
            screen.cursor.bg = bar_bg
            screen.cursor.x = end
            screen.draw(" " * (clock_x - end))

            # Draw the clock
            screen.cursor.x = clock_x
            screen.draw(clock_text)

    return end


def _redraw_tab_bar(_timer_id) -> None:
    tm = get_boss().active_tab_manager
    if tm is not None:
        tm.mark_tab_bar_dirty()
