from plotnine import ggplot, aes, geom_bar, geom_label
from plotnine.stats.stat import stat
from plotnine.utils import after_stat
import pandas as pd
from plotnine.stats.stat import identity


def plotmaker(df, feature):
    plot = (
        ggplot(df, aes("factor(cyl)", fill="factor(am)"))
        + geom_bar(position="fill")
        + geom_label(
            aes(label=after_stat("prop_per_x(x, count) * 100")),
            stat=stat(identity, format_string="{:.1f}%"),
            position="fill",
            size=9,
        )
    )
    return plot
