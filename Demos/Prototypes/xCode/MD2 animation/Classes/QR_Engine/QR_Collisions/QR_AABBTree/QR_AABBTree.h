/******************************************************************************
 * ==> QR_AABBTree -----------------------------------------------------------*
 ******************************************************************************
 * Description : Aligned-axis bounding box tree                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_AABBTreeH
#define QR_AABBTreeH

// qr engine
#include "QR_Types.h"
#include "QR_Vector3D.h"
#include "QR_Plane.h"
#include "QR_Polygon.h"
#include "QR_Sphere.h"
#include "QR_Box.h"
#include "QR_Ray.h"

/**
* Algorithm to use for division
*/
enum EAABBDivisionType
{
    E_AABB_Unknown = 0,
    E_AABB_BestAxis,
    E_AABB_LongestAxis,
};

/**
* Aligned axis bounding box tree info, contains global information about tree
*@author Jean-Milost Reymond
*/
struct QR_AABBTreeInfo
{
    QR_SizeT          m_MaxDeep;
    QR_SizeT          m_MinVertices;
    EAABBDivisionType m_DivisionType;

    QR_AABBTreeInfo();
    virtual ~QR_AABBTreeInfo();

    /**
    * Tests if max deep is reached
    *@param deep - current deep
    *@return true if max deep is reached, otherwise false
    */
    virtual bool TestDeep(QR_SizeT deep) const;

    /**
    * Tests if minimum vertices pes leaf is reached
    *@param vertices - current vertice count
    *@return true if minimum vertices per leaf is reached, otherwise false
    */
    virtual bool TestMinVertices(QR_SizeT vertices) const;
};

/**
* Aligned axis bounding box tree node
*@author Jean-Milost Reymond
*/
struct QR_AABBNode
{
    public:
        /**
        * Plane side result
        */
        enum IE_Side
        {
            IE_Front,
            IE_Back,
            IE_Splitted,
            IE_Coplanar,
        };

        QR_AABBNode*   m_pParent;
        QR_AABBNode*   m_pLeft;
        QR_AABBNode*   m_pRight;
        QR_PolygonList m_PolygonList;
        QR_Box         m_Box;
        QR_PlaneP      m_SplittingPlane;
        QR_UInt32      m_Deep;

        /**
        * Constructor
        *@param pParent - parent node, can be NULL
        */
        QR_AABBNode(QR_AABBNode* pParent);
        virtual ~QR_AABBNode();

        /**
        * Creates children nodes
        *@param deep - tree deep
        *@param info - tree info
        *@param[out] deepCount - deep that children really reached
        *@param[out] childrenCount - children count
        *@return true on success, otherwise false
        */
        virtual bool CreateChildren(QR_UInt32              deep,
                                    const QR_AABBTreeInfo& info,
                                    QR_UInt32&             deepCount,
                                    QR_UInt32&             childrenCount);

        /**
        * Resolves node
        *@param sphere - sphere to test
        *@param[out] polygonList - polygon list containing polygons to check
        *@return true if node is resolved, otherwise false
        */
        virtual bool Resolve(const QR_Sphere& sphere, QR_PolygonList& polygonList) const;

        /**
        * Resolves node
        *@param ray - ray
        *@param[out] polygonList - polygon list containing polygons to check
        *@return true if tree was resolved, otherwise false
        */
        virtual bool Resolve(const QR_RayP& ray, QR_PolygonList& polygonList) const;

    private:
        /**
        * Calculates center of all polygons contained in the list
        *@return polygons center
        */
        QR_Vector3DP FindPolygonsCenter() const;

        /**
        * Finds polygon list bounding box longest axis
        *@return longest axis
        */
        QR_Box::IE_Axis FindLongestAxis() const;

        /**
        * Finds polygon list bounding box best axis for division
        *@return longest axis
        */
        QR_Box::IE_Axis FindBestAxis() const;

        /**
        * Tests on which side of the bounding box plane the polygon is
        *@param pPolygon - polygon to test
        *@param plane - bounding box plane
        *@return side
        */
        IE_Side TestPolygonWithBBoxPlane(const QR_Polygon* pPolygon,
                                         const QR_PlaneP&  plane) const;

        /**
        * Adds polygon to result list
        *@param[in, out] - pPolygonList - polygon list to populate
        *@return true on success, otherwise false
        */
        bool AddPolygons(QR_PolygonList& polygonList) const;
};

/**
* Aligned axis bounding box tree
*@author Jean-Milost Reymond
*/
class QR_AABBTree
{
    public:
        /**
        * Constructor
        */
        QR_AABBTree();

        /**
        * Constructor
        *@param polygonList - polygon list to partition
        */
        QR_AABBTree(const QR_PolygonList& polygonList);

        virtual ~QR_AABBTree();

        /**
        * Gets children count
        *@return children count
        */
        virtual QR_UInt32 GetChildrenCount() const;

        /**
        * Gets deep
        *@return deep
        */
        virtual QR_UInt32 GetDeep() const;

        /**
        * Sets tree information
        *@param info - tree information
        */
        virtual void SetInfo(const QR_AABBTreeInfo& info);

        /**
        * Initializes tree
        *@param polygonList - polygon list to partition
        *@return true on success, otherwise false
        */
        virtual bool Initialize(const QR_PolygonList& polygonList);

        /**
        * Resolves tree
        *@param sphere - sphere to test
        *@param[out] polygonList - resulting polygon list to check
        *@return true if tree was resolved, otherwise false
        */
        virtual bool Resolve(const QR_Sphere& sphere, QR_PolygonList& polygonList) const;

        /**
        * Resolves tree
        *@param r - ray
        *@param[out] polygonList - resulting polygon list to check
        *@return true if tree was resolved, otherwise false
        */
        virtual bool Resolve(const QR_RayP& ray, QR_PolygonList& polygonList) const;

    private:
        QR_AABBNode*    m_pRoot;
        QR_AABBTreeInfo m_Info;
        QR_UInt32       m_Deep;
        QR_UInt32       m_ChildrenCount;
};

#endif // QR_AABBTreeH
