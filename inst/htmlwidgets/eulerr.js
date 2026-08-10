/* Interactive SVG renderer for eulerr diagrams.
 *
 * The R side (`euler_widget()`) hands us device-independent geometry in native
 * coordinates with a y-up convention (like grid). SVG is y-down, so we negate
 * every y as we build path data and set the viewBox's minY to -(ylim max).
 * That single, consistent sign flip is the whole coordinate story.
 *
 * v1 interactivity: hover a region to raise its opacity and show a tooltip
 * with the set combination and quantity. No external JS dependencies.
 */
/* global HTMLWidgets, document */

(function () {
  "use strict";

  var SVGNS = "http://www.w3.org/2000/svg";

  function svgEl(tag) {
    return document.createElementNS(SVGNS, tag);
  }

  // htmlwidgets serializes with auto_unbox = TRUE, so a length-1 numeric vector
  // (e.g. a single-ring region's `id_lengths`) arrives as a scalar rather than
  // an array. Coerce back to an array before iterating.
  function asArray(v) {
    if (v === null || v === undefined) {
      return [];
    }
    return Array.isArray(v) ? v : [v];
  }

  // Build an SVG path "d" from concatenated rings. `idLengths` gives the vertex
  // count of each ring; multiple rings on one path plus fill-rule evenodd punch
  // holes, matching eulerr's winding/hole geometry. y is negated (y-up -> y-down).
  function ringsToPath(x, y, idLengths) {
    x = asArray(x);
    y = asArray(y);
    idLengths = asArray(idLengths);
    var d = "";
    var pos = 0;
    for (var r = 0; r < idLengths.length; r++) {
      var len = idLengths[r];
      for (var i = 0; i < len; i++) {
        var cmd = i === 0 ? "M" : "L";
        d += cmd + x[pos + i] + " " + -y[pos + i] + " ";
      }
      d += "Z ";
      pos += len;
    }
    return d;
  }

  function ensureTooltip(el) {
    var tip = el.querySelector(".eulerr-tooltip");
    if (!tip) {
      tip = document.createElement("div");
      tip.className = "eulerr-tooltip";
      tip.style.position = "absolute";
      tip.style.pointerEvents = "none";
      tip.style.padding = "3px 7px";
      tip.style.borderRadius = "3px";
      tip.style.background = "rgba(0, 0, 0, 0.8)";
      tip.style.color = "#fff";
      tip.style.font = "12px sans-serif";
      tip.style.whiteSpace = "nowrap";
      tip.style.visibility = "hidden";
      tip.style.zIndex = "10";
      el.appendChild(tip);
    }
    return tip;
  }

  function tooltipText(region) {
    if (region.quantity === null || region.quantity === undefined) {
      return region.label;
    }
    return region.label + ": " + region.quantity;
  }

  // Blend a "#rrggbb" color toward black (amount < 0) or white (amount > 0).
  // Used for the hover cue so a region visibly changes even at full opacity.
  function shade(hex, amount) {
    if (typeof hex !== "string" || hex.charAt(0) !== "#" || hex.length < 7) {
      return hex;
    }
    var target = amount < 0 ? 0 : 255;
    var p = Math.abs(amount);
    var out = "#";
    for (var i = 1; i < 7; i += 2) {
      var c = parseInt(hex.substring(i, i + 2), 16);
      c = Math.round(c + (target - c) * p);
      out += ("0" + c.toString(16)).slice(-2);
    }
    return out;
  }

  // baseFill is a "#rrggbb" string, or null for regions with no fill (drawn as
  // transparent hover targets).
  function wireHover(path, region, el, tooltip, baseFill, baseAlpha) {
    path.addEventListener("mouseover", function () {
      if (baseFill === null) {
        // no fill: reveal a faint highlight so the region is discoverable
        path.setAttribute("fill-opacity", 0.2);
      } else {
        path.setAttribute("fill", shade(baseFill, -0.25));
        path.setAttribute("fill-opacity", Math.min(1, baseAlpha + 0.15));
      }
      path.setAttribute("stroke", "#000000");
      path.setAttribute("stroke-width", 1.5);
      path.setAttribute("stroke-opacity", 0.9);
      path.setAttribute("vector-effect", "non-scaling-stroke");
      tooltip.textContent = tooltipText(region);
      tooltip.style.visibility = "visible";
    });
    path.addEventListener("mousemove", function (ev) {
      var rect = el.getBoundingClientRect();
      tooltip.style.left = ev.clientX - rect.left + 10 + "px";
      tooltip.style.top = ev.clientY - rect.top + 10 + "px";
    });
    path.addEventListener("mouseout", function () {
      if (baseFill === null) {
        path.setAttribute("fill-opacity", 0);
      } else {
        path.setAttribute("fill", baseFill);
        path.setAttribute("fill-opacity", baseAlpha);
      }
      path.setAttribute("stroke", "none");
      tooltip.style.visibility = "hidden";
    });
  }

  function addText(svg, x, y, text, col, fontSizePx) {
    var t = svgEl("text");
    t.setAttribute("x", x);
    t.setAttribute("y", -y);
    t.setAttribute("text-anchor", "middle");
    t.setAttribute("dominant-baseline", "central");
    t.setAttribute("fill", col);
    t.setAttribute("font-family", "sans-serif");
    t.setAttribute("font-size", fontSizePx);
    t.style.pointerEvents = "none";
    var lines = String(text).split("\n");
    var lineHeight = fontSizePx * 1.2;
    var y0 = -(lines.length - 1) / 2 * lineHeight;
    for (var i = 0; i < lines.length; i++) {
      var span = svgEl("tspan");
      span.setAttribute("x", x);
      span.setAttribute("dy", i === 0 ? y0 : lineHeight);
      span.textContent = lines[i];
      t.appendChild(span);
    }
    svg.appendChild(t);
  }

  HTMLWidgets.widget({
    name: "eulerr",
    type: "output",

    factory: function (el, width) {
      return {
        renderValue: function (d) {
          // clear previous render (keep nothing)
          el.innerHTML = "";
          el.style.position = "relative";

          var xlim = d.xlim;
          var ylim = d.ylim;
          var w = xlim[1] - xlim[0];
          var h = ylim[1] - ylim[0];
          var pad = 0.02 * Math.max(w, h);

          // scale from user units to px for constant on-screen font sizes
          var px = el.clientWidth || width || 400;
          var unitsPerPx = (w + 2 * pad) / px;

          var svg = svgEl("svg");
          svg.setAttribute(
            "viewBox",
            (xlim[0] - pad) +
              " " +
              (-ylim[1] - pad) +
              " " +
              (w + 2 * pad) +
              " " +
              (h + 2 * pad)
          );
          svg.setAttribute("preserveAspectRatio", "xMidYMid meet");
          svg.setAttribute("width", "100%");
          svg.setAttribute("height", "100%");
          svg.style.display = "block";

          var tooltip = ensureTooltip(el);

          // 1) container/complement box (bottom)
          if (d.container) {
            var c = d.container;
            if (c.complement_x && c.complement_x.length && c.fill !== "transparent") {
              var cfill = svgEl("path");
              cfill.setAttribute(
                "d",
                ringsToPath(c.complement_x, c.complement_y, c.complement_id_lengths)
              );
              cfill.setAttribute("fill", c.fill);
              cfill.setAttribute("fill-opacity", c.alpha);
              cfill.setAttribute("fill-rule", "evenodd");
              cfill.style.pointerEvents = "none";
              svg.appendChild(cfill);
            }
            var box = svgEl("path");
            box.setAttribute(
              "d",
              ringsToPath(c.outline_x, c.outline_y, [c.outline_x.length])
            );
            box.setAttribute("fill", "none");
            box.setAttribute("stroke", c.col);
            box.setAttribute("stroke-width", c.lwd);
            box.setAttribute("vector-effect", "non-scaling-stroke");
            if (c.lty === 2) {
              box.setAttribute("stroke-dasharray", "4 4");
            }
            svg.appendChild(box);
          }

          // 2) region fills (hover targets)
          d.regions.forEach(function (r) {
            var p = svgEl("path");
            p.setAttribute("d", ringsToPath(r.x, r.y, r.id_lengths));
            var baseFill = r.fill === undefined ? null : r.fill;
            var baseAlpha = r.alpha;
            if (baseFill === null) {
              p.setAttribute("fill", "#000000");
              p.setAttribute("fill-opacity", 0);
              p.style.pointerEvents = "all"; // hoverable even without a fill
              baseAlpha = 0;
            } else {
              p.setAttribute("fill", baseFill);
              p.setAttribute("fill-opacity", baseAlpha);
            }
            p.setAttribute("fill-rule", "evenodd");
            wireHover(p, r, el, tooltip, baseFill, baseAlpha);
            svg.appendChild(p);
          });

          // 3) set edges (on top of fills)
          d.edges.forEach(function (e) {
            var p = svgEl("path");
            p.setAttribute("d", ringsToPath(e.x, e.y, [e.x.length]));
            p.setAttribute("fill", "none");
            p.setAttribute("stroke", e.col);
            p.setAttribute("stroke-width", e.lwd);
            p.setAttribute("vector-effect", "non-scaling-stroke");
            p.style.pointerEvents = "none";
            svg.appendChild(p);
          });

          // 4) leader lines + labels (topmost)
          d.labels.forEach(function (l) {
            if (l.leader) {
              var pts = [[l.leader.x0, l.leader.y0]];
              var wx = asArray(l.leader.wx);
              var wy = asArray(l.leader.wy);
              for (var k = 0; k < wx.length; k++) {
                pts.push([wx[k], wy[k]]);
              }
              pts.push([l.leader.x1, l.leader.y1]);
              var dd = "";
              for (var i = 0; i < pts.length; i++) {
                dd += (i === 0 ? "M" : "L") + pts[i][0] + " " + -pts[i][1] + " ";
              }
              var line = svgEl("path");
              line.setAttribute("d", dd);
              line.setAttribute("fill", "none");
              line.setAttribute("stroke", l.col);
              line.setAttribute("stroke-width", 1);
              line.setAttribute("vector-effect", "non-scaling-stroke");
              line.style.pointerEvents = "none";
              svg.appendChild(line);
            }
            addText(svg, l.x, l.y, l.text, l.col, l.fontsize * unitsPerPx);
          });

          if (d.container && d.container.label_text) {
            addText(
              svg,
              d.container.label_x,
              d.container.label_y,
              d.container.label_text,
              d.container.label_col,
              d.container.fontsize * unitsPerPx
            );
          }

          el.appendChild(svg);
        },

        resize: function () {
          // viewBox + preserveAspectRatio handle scaling; nothing to redo.
        }
      };
    }
  });
})();
