#include <RcppArmadillo.h>
#include "geometry.h"

// circleIntersection.subdivideRectangle = function(current, output) {
//   var w = current.width/2,
//     h = current.height/2,
//     level = current.level || 0;
//
//   output({ x: current.x,
//          y : current.y,
//          width : w,
//          height: h,
//          level : level + 1});
//
//   output({ x: current.x + w,
//          y : current.y,
//          width : w,
//          height: h,
//          level : level + 1});
//
//   output({ x: current.x,
//          y : current.y + h,
//          width : w,
//          height: h,
//          level : level + 1});
//
//   output({ x: current.x + w,
//          y : current.y + h,
//          width : w,
//          height: h,
//          level : level + 1});
// }

struct box {
  double x0, y0, x1, y1;
  unsigned int lvl;

  double width(void) {return x1 - x0;};
  double area(void) {return width()*width();};
  void level_up(void) {++lvl;}

  box(double x0, double y0, double x1, double y1, unsigned int lvl) {
    x0 = x0;
    y0 = y0;
    x1 = x1;
    y1 = y1;
    lvl = lvl;
  };
};

// arma::mat
// bounding_box(arma::mat& ellipses) {
//
//   arma::vec xlim = arma::sqrt(a%a%arma::pow(arma::cos(phi), 2) +
//     b%b%arma::pow(arma::sin(phi), 2));
//   arma::vec ylim = arma::sqrt(a%a%arma::pow(arma::sin(phi), 2) +
//     b%b%arma::pow(arma::cos(phi), 2));
//
//   arma::vec xlim_lo = h + xlim;
//   arma::vec xlim_hi = h - xlim;
//   arma::vec ylim_lo = k + ylim;
//   arma::vec ylim_hi = k - ylim;
//
//   arma::mat::fixed<2, 2> corners;
//   corners.col(0) = arma::range(arma::join_vert(xlim_lo, xlim_hi));
//   corners.col(1) = arma::range(arma::join_vert(ylim_lo, ylim_hi));
//
//   return corners;
// }
arma::umat
  scrutinize_box(box bounds, const arma::mat& ellipses) {
    arma::rowvec::fixed<4> x;
    arma::rowvec::fixed<4> y;

    x(0) = bounds.x0;
    x(1) = bounds.x0;
    x(2) = bounds.x1;
    x(3) = bounds.x1;
    y(0) = bounds.y0;
    y(1) = bounds.y1;
    y(2) = bounds.y0;
    y(3) = bounds.y1;

    return find_surrounding_sets(x, y,
                                 ellipses.row(0).t(),
                                 ellipses.row(1).t(),
                                 ellipses.row(2).t(),
                                 ellipses.row(3).t(),
                                 ellipses.row(4).t());
  }

void branch_box(box bounds,
                arma::mat& ellipses,
                arma::vec& areas,
                arma::umat& id,
                double& outside_area) {
  double w = bounds.width()/2;
  unsigned int lvl  = bounds.lvl + 1;

  if (lvl < 9) {
    arma::umat set_parents = scrutinize_box(bounds, ellipses);

    if (arma::any(arma::vectorise(set_parents))) {
      arma::uvec ind = (arma::sum(set_parents, 1) == ellipses.n_rows);

      arma::sword area_ind = -1;
      for (arma::uword i = 0; i < id.n_rows; ++i) {
        if (arma::all(id.row(0) == ind.t())) {
          area_ind = i;
          break;
        }
      }

      if (area_ind == -1) {
        box ll(bounds.x0, bounds.y0, bounds.x0 + w, bounds.y0 + w, lvl);
        box lr(bounds.x0 + w, bounds.y0, bounds.x1, bounds.y0 + w, lvl);
        box ul(bounds.x0, bounds.y0 + w, bounds.x0 + w, bounds.y1, lvl);
        box ur(bounds.x0 + w, bounds.y0 + w, bounds.x1, bounds.y1, lvl);

        branch_box(ll, ellipses, areas, id, outside_area);
        branch_box(lr, ellipses, areas, id, outside_area);
        branch_box(ul, ellipses, areas, id, outside_area);
        branch_box(ur, ellipses, areas, id, outside_area);
      } else {
        areas(area_ind) += w*w;
      }
    } else {
      outside_area += w*w;
    }
  }
}


// [[Rcpp::export]]
arma::vec quad_tree(arma::mat ellipses,
                    arma::umat id) {
  arma::vec h = ellipses.row(0).t();
  arma::vec k = ellipses.row(1).t();
  arma::vec a = ellipses.row(2).t();
  arma::vec b = ellipses.row(3).t();
  arma::vec phi = ellipses.row(4).t();
  arma::vec areas(id.n_rows, arma::fill::zeros);

  arma::vec xlim = arma::sqrt(a%a%arma::pow(arma::cos(phi), 2) +
    b%b%arma::pow(arma::sin(phi), 2));
  arma::vec ylim = arma::sqrt(a%a%arma::pow(arma::sin(phi), 2) +
    b%b%arma::pow(arma::cos(phi), 2));

  arma::vec xrng = arma::join_vert(h + xlim, h - xlim);
  arma::vec yrng = arma::join_vert(k + ylim, k - ylim);

  box bounds(arma::min(xrng),
             arma::min(yrng),
             arma::max(xrng),
             arma::max(yrng),
             1);

  double outside_area = 0.0;

  ellipses.print();

  branch_box(bounds, ellipses, areas, id, outside_area);;

  return std::pow(bounds.width(), 2)/(arma::accu(areas) + outside_area)*areas;
}

//
// circleIntersection.rectangleContained = function(current, circles) {
//   var x = current.x, y = current.y, w = current.width, h = current.height;
//
//   var pointValues = [
//   circleIntersection.containedInCircles({x:x, y:y} , circles),
//   circleIntersection.containedInCircles({x:x+w, y:y} , circles),
//   circleIntersection.containedInCircles({x:x, y:y+h} , circles),
//   circleIntersection.containedInCircles({x:x+w, y:y+h}, circles)];
//
//   for (var i = 1; i < pointValues.length; ++i) {
//     if (pointValues[i] !== pointValues[0]) {
//       return 0;
//     }
//   }
//
//   return pointValues[0] ? 1 : -1;
// }
//
//
// circleIntersection.quadtreeEstimate = function(circles, depth) {
//   var bound = circleIntersection.getBoundingRectangle(circles),
//     area = 0, outsideArea = 0;
//
//   if (bound.widh <= 0 || bound.height <= 0) {
//     return [0,0];
//   }
//
//   depth = depth || 8;
//
//   function examineRectangle(r) {
//     var inOrOut = circleIntersection.rectangleContained(r, circles);
//     if (inOrOut === 0) {
//       if (r.level <= depth) {
//         circleIntersection.subdivideRectangle(r, examineRectangle);
//       }
//     } else if (inOrOut > 0) {
//       area += r.width * r.height;
//     } else {
//       outsideArea += r.width * r.height;
//     }
//   }
//
//   bound.level = 0;
//   circleIntersection.subdivideRectangle(bound, examineRectangle);
//
//   var uncertain = (bound.width * bound.height - area - outsideArea)/2;
//   return [area + uncertain, uncertain];
// };
//
// /** returns whether a point is contained by all of a list of circles */
// circleIntersection.containedInCircles = function(point, circles) {
//   for (var i = 0; i < circles.length; ++i) {
//     if (circleIntersection.distance(point, circles[i]) > circles[i].radius + SMALL) {
//       return false;
//     }
//   }
//   return true;
// };
