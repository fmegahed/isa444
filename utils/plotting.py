# Based on https://stackoverflow.com/a/73300850
def plot_acf_colors(ax, markercolor="#C3142D", linecolor="black", facecolor="silver", barcolor="#C3142D", linewidth=1, alpha=0.5):
    """utility function to get some control over colors with  plot_acf()"""
    import matplotlib.pyplot as plt
    from statsmodels.graphics.tsaplots import plot_acf
    from matplotlib.collections import PolyCollection, LineCollection
    
    for item in ax.collections:
        # change the color of the confidence interval
        if isinstance(item, PolyCollection): #
            item.set_facecolor(facecolor)
            item.set_alpha(alpha) # add alpha control (minor).
        # change the color of the vertical lines
        if isinstance(item, LineCollection):
            item.set_color(barcolor)
    # change the color of the markers
    [line.get_label() for line in ax.lines]
    for item in ax.lines:
        item.set_color(markercolor)
    # change the color of the horizontal lines
    ax.lines[0].set_color(linecolor)
    ax.lines[0].set_linewidth(linewidth)
    #ax.lines.remove(ax.lines[0])
    return ax

