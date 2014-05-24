#include "../include/chull.h"
#include <pcl/io/io.h>
#include <pcl/io/pcd_io.h>
#include <pcl/features/integral_image_normal.h>
#include <pcl/visualization/cloud_viewer.h>
#include <pcl/point_types.h>
#include <pcl/features/normal_3d.h>
#include <pcl/kdtree/kdtree_flann.h>
#include <pcl/common/centroid.h>
#include <pcl/surface/convex_hull.h>

void convexhull(double *in, int n, double *out, int *resNum) {
    pcl::PointCloud<pcl::PointXYZ>::Ptr cloud (new pcl::PointCloud<pcl::PointXYZ>);
    // Fill in the cloud data 
    cloud->width    = n; 
    cloud->height   = 1; 
    cloud->is_dense = false; 
    cloud->points.resize (cloud->width * cloud->height);

    for (size_t i = 0; i < cloud->points.size (); ++i) { 
	cloud->points[i].x = in[i * 3]; 
	cloud->points[i].y = in[i * 3 + 1]; 
	cloud->points[i].z = in[i * 3 + 2]; 
    }


    pcl::ConvexHull<pcl::PointXYZ> cHull;
    pcl::PointCloud<pcl::PointXYZ> cHull_points;
    
    cHull.setInputCloud(cloud);
    cHull.reconstruct (cHull_points);
    for (size_t i = 0; i < cHull_points.points.size (); ++i) { 
	out[i * 3 + 0] = cHull_points.points[i].x; 
	out[i * 3 + 1] = cHull_points.points[i].y; 
	out[i * 3 + 2] = cHull_points.points[i].z; 
    }

    *resNum = cHull_points.points.size();
    
}
