/******************************************************************************
 * ==> QR_AABBTree -----------------------------------------------------------*
 ******************************************************************************
 * Description : Aligned-axis bounding box tree                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_AABBTree.h"

// std
#include <stddef.h>
#include <cmath>
#include <memory>

// qr engine
#include "QR_Exception.h"
#include "QR_3DCollisionHelper.h"

//------------------------------------------------------------------------------
// Class QR_AABBTreeInfo - c++ cross-platform
//------------------------------------------------------------------------------
QR_AABBTreeInfo::QR_AABBTreeInfo() :
    m_MaxDeep(0),
    m_MinVertices(0),
    m_DivisionType(E_AABB_LongestAxis)
{}
//------------------------------------------------------------------------------
QR_AABBTreeInfo::~QR_AABBTreeInfo()
{}
//------------------------------------------------------------------------------
bool QR_AABBTreeInfo::TestDeep(QR_UInt32 deep) const
{
    // do ignore test?
    if (!m_MaxDeep)
        return false;

    // is max deep reached?
    return (deep >= m_MaxDeep);
}
//------------------------------------------------------------------------------
bool QR_AABBTreeInfo::TestMinVertices(QR_UInt32 vertices) const
{
    // node contains less vertices than minimum?
    return (vertices < m_MinVertices);
}
//------------------------------------------------------------------------------
// Class QR_AABBNode - c++ cross-platform
//------------------------------------------------------------------------------
QR_AABBNode::QR_AABBNode(QR_AABBNode* pParent) :
    m_pParent(pParent),
    m_pLeft(NULL),
    m_pRight(NULL),
    m_Deep(0)
{}
//------------------------------------------------------------------------------
QR_AABBNode::~QR_AABBNode()
{
    // delete left node
    if (m_pLeft)
        delete m_pLeft;

    // delete right node
    if (m_pRight)
        delete m_pRight;
}
//------------------------------------------------------------------------------
bool QR_AABBNode::CreateChildren(QR_UInt32              deep,
                                 const QR_AABBTreeInfo& info,
                                 QR_UInt32&             deepCount,
                                 QR_UInt32&             childrenCount)
{
    // calculate bounding box
    m_Box = QR_3DCollisionHelper::CalculateBoundingBox(m_PolygonList);

    // is max deep reached?
    if (info.TestDeep(deep))
        return true;

    QR_Box::IE_Axis axis;

    // get division axis using appropriate algorithm
    switch (info.m_DivisionType)
    {
        case E_AABB_BestAxis:
            // search for best axis
            axis = FindBestAxis();
            break;

        case E_AABB_LongestAxis:
            // search for longest axis
            axis = FindLongestAxis();
            break;

        case E_AABB_Unknown:
        default:
            return false;
    }

    // calculate polygons center
    QR_Vector3DP polysCenter = FindPolygonsCenter();

    // calculate splitting plane
    m_SplittingPlane = m_Box.GetSplittingPlane(axis, polysCenter);

    // create children nodes
    std::auto_ptr<QR_AABBNode> pLeft(new QR_AABBNode(this));
    std::auto_ptr<QR_AABBNode> pRight(new QR_AABBNode(this));

    // iterate through polygons
    for (QR_PolygonList::const_iterator it = m_PolygonList.begin();
            it != m_PolygonList.end(); ++it)
        // determine on which plane side the polygon is and dispatch result
        switch (TestPolygonWithBBoxPlane((*it), m_SplittingPlane))
        {
            case IE_Back:
                // add polygon in left child
                pLeft->m_PolygonList.push_back(*it);
                break;

            case IE_Front:
                // add polygon in right child
                pRight->m_PolygonList.push_back(*it);
                break;

            case IE_Splitted:
            case IE_Coplanar:
                // add polygon in both left and right children
                pLeft->m_PolygonList.push_back(*it);
                pRight->m_PolygonList.push_back(*it);
                break;
        }

    // get polygon count
    const QR_SizeT polygonCount      = m_PolygonList.size();
    const QR_SizeT leftPolygonCount  = pLeft->m_PolygonList.size();
    const QR_SizeT rightPolygonCount = pRight->m_PolygonList.size();

    // is left leaf reached?
    if (!pLeft->m_PolygonList.empty()    &&
        polygonCount != leftPolygonCount &&
        !info.TestMinVertices(leftPolygonCount))
    {
        // try to create left child node
        if (!pLeft->CreateChildren(deep + 1, info, deepCount, childrenCount))
            return false;

        m_pLeft = pLeft.release();

        // update deep and children count
        deepCount = std::max(deepCount, deep);
        ++childrenCount;
    }

    // is right leaf reached?
    if (!pRight->m_PolygonList.empty()    &&
        polygonCount != rightPolygonCount &&
        !info.TestMinVertices(rightPolygonCount))
    {
        // try to create right child
        if (!pRight->CreateChildren(deep + 1, info, deepCount, childrenCount))
            return false;

        m_pRight = pRight.release();

        // update deep and children count
        deepCount = std::max(deepCount, deep);
        ++childrenCount;
    }

    return true;
}
//------------------------------------------------------------------------------
bool QR_AABBNode::Resolve(const QR_Sphere& sphere, QR_PolygonList& polygonList) const
{
    // is sphere colliding node box?
    if (!QR_3DCollisionHelper::GetSphereBoxCollision(sphere, m_Box))
        return false;

    // is leaf?
    if (!m_pLeft && !m_pRight)
        // add polygons to check in resulting list
        return AddPolygons(polygonList);

    // todo -cImprovement -oJean: Why not compare with bounding box directly, as it's done for rays?
    //                            This would make the detection simpler and faster, and will improve
    //                            the number of returned polygons...
    // calculate distance between sphere center and splitting plane
    M_Precision dist = m_SplittingPlane.DistanceTo(sphere.m_Center);

    // check sphere position against division plane
    if (dist == 0.0f || QR_3DCollisionHelper::GetSpherePlaneCollision(sphere, m_SplittingPlane))
    {
        bool leftResolved  = false;
        bool rightResolved = false;

        // resolve left node
        if (!m_pLeft || m_pLeft->Resolve(sphere, polygonList))
            leftResolved = true;

        // resolve right node
        if (!m_pRight || m_pRight->Resolve(sphere, polygonList))
            rightResolved = true;

        return (leftResolved || rightResolved);
    }
    else
    if (dist < 0.0f)
    {
        // resolve left node
        if (m_pLeft && !m_pLeft->Resolve(sphere, polygonList))
            return false;
    }
    else
        // resolve right node
        if (m_pRight && !m_pRight->Resolve(sphere, polygonList))
            return false;

    return true;
}
//------------------------------------------------------------------------------
bool QR_AABBNode::Resolve(const QR_RayP& ray, QR_PolygonList& polygonList) const
{
    // is leaf?
    if (!m_pLeft && !m_pRight)
        // add polygons to check in resulting list
        return AddPolygons(polygonList);

    bool leftResolved  = false;
    bool rightResolved = false;

    // check if ray intersects the left box
    if (m_pLeft && QR_3DCollisionHelper::GetRayBoxCollision(ray, m_pLeft->m_Box))
        // resolve left node
        leftResolved = m_pLeft->Resolve(ray, polygonList);

    // check if ray intersects the right box
    if (m_pRight && QR_3DCollisionHelper::GetRayBoxCollision(ray, m_pRight->m_Box))
        // resolve right node
        rightResolved = m_pRight->Resolve(ray, polygonList);

    return (leftResolved || rightResolved);
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_AABBNode::FindPolygonsCenter() const
{
    QR_Vector3DP center;

    // is polygon list empty?
    if (m_PolygonList.empty())
        return center;

    // iterate through polygons
    for (QR_PolygonList::const_iterator it = m_PolygonList.begin();
            it != m_PolygonList.end(); ++it)
        // add polygon center to result
        center += (*it)->GetCenter();

    // calculate center average
    center /= m_PolygonList.size();

    return center;
}
//------------------------------------------------------------------------------
QR_Box::IE_Axis QR_AABBNode::FindLongestAxis() const
{
    // get longest axis from polygon list bounding box
    return m_Box.GetLongestAxis();
}
//------------------------------------------------------------------------------
QR_Box::IE_Axis QR_AABBNode::FindBestAxis() const
{
    // get polygon list bounding box center
    QR_Vector3DP center = m_Box.GetCenter();

    M_Precision boxCenter[3];
    boxCenter[0] = center.m_X;
    boxCenter[1] = center.m_Y;
    boxCenter[2] = center.m_Z;

    QR_Int64 leftCounter[3];
    QR_Int64 rightCounter[3];

    // initialize counters
    std::memset(&leftCounter,  0x0, 3 * sizeof(QR_Int64));
    std::memset(&rightCounter, 0x0, 3 * sizeof(QR_Int64));

    // iterate through polygons
    for (QR_PolygonList::const_iterator it = m_PolygonList.begin();
            it != m_PolygonList.end(); ++it)
    {
        // add polygon center to result
        QR_Vector3DP center = (*it)->GetCenter();

        M_Precision polygonCenter[3];
        polygonCenter[0] = center.m_X;
        polygonCenter[1] = center.m_Y;
        polygonCenter[2] = center.m_Z;

        // iterate through axis
        for (QR_UInt8 axis = 0; axis < 3; ++axis)
            // on each axis, search if polygon center is before or after box center
            if (polygonCenter[axis] <= boxCenter[axis])
                // polygon center is before box center, increment left counter
                ++leftCounter[axis];
            else
                // polygon center is after box center, increment right counter
                ++rightCounter[axis];
    }

    QR_UInt8  index  = 0;
    QR_UInt32 result = 0;

    // iterate through axis
    for (QR_UInt8 axis = 0; axis < 3; ++axis)
    {
        // calculate axis result
        QR_UInt32 axisResult = std::abs(leftCounter[axis] - rightCounter[axis]);

        // is current axis result higher than previous result?
        if (axisResult < result)
        {
            // update result and store index
            result = axisResult;
            index  = axis;
        }
    }

    // search for resulting index
    switch (index)
    {
        case 0:
            return QR_Box::IE_X;

        case 1:
            return QR_Box::IE_Y;

        case 2:
            return QR_Box::IE_Z;

        default:
            return QR_Box::IE_X;
    }
}
//------------------------------------------------------------------------------
QR_AABBNode::IE_Side QR_AABBNode::TestPolygonWithBBoxPlane(const QR_Polygon* pPolygon,
                                                           const QR_PlaneP&  plane) const
{
    M_Precision result[3];

    // iterate through polygon vertex
    for (QR_UInt8 i = 0; i < 3; ++i)
    {
        // get vertex
        const QR_Vector3DP vertex = pPolygon->GetVertex(i);

        // get distance between polygon vertex and division plane
        result[i] = plane.DistanceTo(vertex);
    }

    // search for which plane side the polygon is
    if (result[0] == 0.0f && result[1] == 0.0f && result[2] == 0.0f)
        return IE_Coplanar;
    else
    if (result[0] <= 0.0f && result[1] <= 0.0f && result[2] <= 0.0f)
        return IE_Back;
    else
    if (result[0] >= 0.0f && result[1] >= 0.0f && result[2] >= 0.0f)
        return IE_Front;
    else
        return IE_Splitted;
}
//------------------------------------------------------------------------------
bool QR_AABBNode::AddPolygons(QR_PolygonList& polygonList) const
{
    // iterate through polygons
    for (QR_PolygonList::const_iterator it = m_PolygonList.begin();
            it != m_PolygonList.end(); ++it)
    {
        bool exists = false;

        // iterate through already added polygons
        for (QR_PolygonList::const_iterator itAdded = polygonList.begin();
                itAdded != polygonList.end(); ++itAdded)
            // polygon already added in list?
            if ((*itAdded) == (*it))
            {
                exists = true;
                break;
            }

        // already added?
        if (exists)
            continue;

        polygonList.push_back(*it);
    }

    return true;
}
//------------------------------------------------------------------------------
// Class QR_AABBTree - c++ cross-platform
//------------------------------------------------------------------------------
QR_AABBTree::QR_AABBTree() :
    m_pRoot(NULL),
    m_Deep(0),
    m_ChildrenCount(0)
{}
//------------------------------------------------------------------------------
QR_AABBTree::QR_AABBTree(const QR_PolygonList& polygonList) :
    m_pRoot(NULL),
    m_Deep(0),
    m_ChildrenCount(0)
{
    // try to initialize tree with polygon list
    if (!Initialize(polygonList))
        M_THROW_EXCEPTION("Could not initialize tree");
}
//------------------------------------------------------------------------------
QR_AABBTree::~QR_AABBTree()
{
    // delete tree children
    if (m_pRoot)
        delete m_pRoot;
}
//------------------------------------------------------------------------------
QR_UInt32 QR_AABBTree::GetChildrenCount() const
{
    return m_ChildrenCount;
}
//------------------------------------------------------------------------------
QR_UInt32 QR_AABBTree::GetDeep() const
{
    return m_Deep;
}
//------------------------------------------------------------------------------
void QR_AABBTree::SetInfo(const QR_AABBTreeInfo& info)
{
    m_Info = info;
}
//------------------------------------------------------------------------------
bool QR_AABBTree::Initialize(const QR_PolygonList& polygonList)
{
    m_Deep          = 0;
    m_ChildrenCount = 0;

    // delete previous tree children, if exists
    if (m_pRoot)
    {
        delete m_pRoot;
        m_pRoot = NULL;
    }

    // create and populate root node
    std::auto_ptr<QR_AABBNode> pRoot(new QR_AABBNode(NULL));
    pRoot->m_PolygonList = polygonList;

    // try to create tree children
    if (!pRoot->CreateChildren(0, m_Info, m_Deep, m_ChildrenCount))
        return false;

    m_pRoot = pRoot.release();

    return true;
}
//------------------------------------------------------------------------------
bool QR_AABBTree::Resolve(const QR_Sphere& sphere, QR_PolygonList& polygonList) const
{
    // root node exists?
    if (!m_pRoot)
        return false;

    // resolve children
    return m_pRoot->Resolve(sphere, polygonList);
}
//------------------------------------------------------------------------------
bool QR_AABBTree::Resolve(const QR_RayP& ray, QR_PolygonList& polygonList) const
{
    // root node exists?
    if (!m_pRoot)
        return false;

    // resolve children
    return m_pRoot->Resolve(ray, polygonList);
}
//------------------------------------------------------------------------------
