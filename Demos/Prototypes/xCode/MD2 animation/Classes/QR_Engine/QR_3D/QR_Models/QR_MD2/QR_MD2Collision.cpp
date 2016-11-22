/******************************************************************************
 * ==> QR_MD2Collision -------------------------------------------------------*
 ******************************************************************************
 * Description : Collision data to use with a MD2 model                       *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD2Collision.h"

// qr engine
#include "QR_STDTools.h"
#include "QR_3DCollisionHelper.h"

//------------------------------------------------------------------------------
// QR_MD2Collision - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2Collision::QR_MD2Collision()
{}
//------------------------------------------------------------------------------
QR_MD2Collision::~QR_MD2Collision()
{}
//------------------------------------------------------------------------------
bool QR_MD2Collision::ResolveAnimation(const QR_Double&       elapsedTime,
                                             QR_SizeT         fps,
                                             QR_MD2Animation* pAnimation,
                                       const QR_RayP&         ray,
                                             QR_PolygonList&  polygonList) const
{
    // no animation?
    if (!pAnimation)
        return false;

    // get animation
    const QR_MD2Animation::IAnimation animation = pAnimation->GetAnimation(elapsedTime, fps);

    // found it?
    if (animation.m_FrameIndex == M_MD2_Invalid_Index)
        return false;

    QR_PolygonList firstList;
    QR_PolygonList secondList;
    bool           success = false;

    try
    {
        // get polygons to test from current frame and frame to interpolate with
        success  = GetPolygonsToCheck(animation.m_FrameIndex,              ray, firstList);
        success &= GetPolygonsToCheck(animation.m_InterpolationFrameIndex, ray, secondList);

        // succeeded?
        if (!success)
            return false;

        QR_PolygonList firstPolygons;

        // interpolate all polygons found in the first frame with the second frame
        InterpolateAndGetPolygonsToCheck(animation,
                                         m_Frames[animation.m_FrameIndex],
                                         firstList,
                                         firstPolygons);

        QR_PolygonList secondPolygons;

        // interpolate all polygons found in the second frame with the first frame
        InterpolateAndGetPolygonsToCheck(animation,
                                         m_Frames[animation.m_InterpolationFrameIndex],
                                         secondList,
                                         secondPolygons);

        QR_PolygonList garbage;
        bool           collision = false;

        // iterate through polygons to check in the first frame
        for (QR_PolygonList::iterator it = firstPolygons.begin(); it != firstPolygons.end(); ++it)
            // is polygon intersecting ray?
            if (QR_3DCollisionHelper::GetTriRayCollision(ray, *(*it)))
            {
                // add polygon to resulting list
                polygonList.push_back(*it);
                collision = true;
            }
            else
                // polygon will no more be used, add it to garbage list
                garbage.push_back(*it);

        // iterate through polygons to check in the second frame
        for (QR_PolygonList::iterator it = secondPolygons.begin(); it != secondPolygons.end(); ++it)
            // is polygon intersecting ray?
            if (QR_3DCollisionHelper::GetTriRayCollision(ray, *(*it)))
            {
                bool exists = false;

                // iterate through polygons already in collision
                for (QR_PolygonList::iterator itRes = polygonList.begin(); itRes != polygonList.end();
                        ++itRes)
                {
                    bool isEqual = true;

                    // iterate through each polygon vertex
                    for (QR_UInt8 i = 0; i < 3; ++i)
                        // is second frame polygon vertex not equal to polygon vertex in collision?
                        if ((*it)->GetVertex(i) != (*itRes)->GetVertex(i))
                        {
                            isEqual = false;
                            break;
                        }

                    // found identical polygon in collision list?
                    if (isEqual)
                    {
                        exists = true;
                        break;
                    }
                }

                // polygon was already added?
                if (exists)
                {
                    // skip it and add it to garbage list, as it will no more be used
                    garbage.push_back(*it);
                    continue;
                }

                // add polygon to collision list
                polygonList.push_back(*it);
                collision = true;
            }
            else
                // polygon will no more be used, add it to garbage list
                garbage.push_back(*it);

        // delete all no more used polygons
        QR_STDTools::DelAndClear(garbage);

        return collision;
    }
    catch (...)
    {}

    return false;
}
//------------------------------------------------------------------------------
void QR_MD2Collision::InterpolateAndGetPolygonsToCheck(const QR_MD2Animation::IAnimation& animation,
                                                       const ICollideItem*                pItem,
                                                       const QR_PolygonList&              polygonsToCheck,
                                                             QR_PolygonList&              polygons) const
{
    M_ASSERT(pItem);

    // get first and second frames
    const ICollideItem* pFirstItem  = m_Frames[animation.m_FrameIndex];
    const ICollideItem* pSecondItem = m_Frames[animation.m_InterpolationFrameIndex];

    // iterate through polygons to check
    for (QR_PolygonList::const_iterator it = polygonsToCheck.begin(); it != polygonsToCheck.end(); ++it)
    {
        // get indexed polygon to check
        QR_IndexedPolygon* pPolygon = static_cast<QR_IndexedPolygon*>(*it);

        // found it?
        if (!pPolygon)
            continue;

        QR_SizeT                  v1i;
        QR_SizeT                  v2i;
        QR_SizeT                  v3i;
        const QR_Vertex::IBuffer* pvBuffer;

        // get polygon data
        pPolygon->GetData(v1i, v2i, v3i, pvBuffer);

        // get mesh count
        const QR_SizeT meshCount = pItem->m_Mesh->size();
              QR_SizeT index     = 0;
              bool     found     = false;

        // iterate through mesh vertices containing source vertex buffers to compare with
        for (QR_SizeT j = 0; j < meshCount; ++j)
            // vertex to extract polygon from was found?
            if (&(*pItem->m_Mesh)[j]->m_Buffer == pvBuffer)
            {
                index = j;
                found = true;
                break;
            }

        // found vertex?
        if (!found)
            continue;

        // create interpolated polygon to use for collision detection
        std::auto_ptr<QR_SimplePolygon> pInterpolatedPolygon
                (CreateInterpolatedPolygon(v1i,
                                           v2i,
                                           v3i,
                                           (*pFirstItem->m_Mesh)[index]->m_Buffer,
                                           (*pSecondItem->m_Mesh)[index]->m_Buffer,
                                           animation.m_InterpolationFactor));

        // add it to resulting list
        polygons.push_back(pInterpolatedPolygon.get());
        pInterpolatedPolygon.release();
    }
}
//------------------------------------------------------------------------------
QR_SimplePolygon* QR_MD2Collision::CreateInterpolatedPolygon(      QR_SizeT            i1,
                                                                   QR_SizeT            i2,
                                                                   QR_SizeT            i3,
                                                             const QR_Vertex::IBuffer& buffer1,
                                                             const QR_Vertex::IBuffer& buffer2,
                                                                   QR_Double           factor) const
{
    // get vertices to interpolate from buffers
    QR_Vector3DP v1a(buffer1[i1], buffer1[i1 + 1], buffer1[i1 + 2]);
    QR_Vector3DP v2a(buffer1[i2], buffer1[i2 + 1], buffer1[i2 + 2]);
    QR_Vector3DP v3a(buffer1[i3], buffer1[i3 + 1], buffer1[i3 + 2]);
    QR_Vector3DP v1b(buffer2[i1], buffer2[i1 + 1], buffer2[i1 + 2]);
    QR_Vector3DP v2b(buffer2[i2], buffer2[i2 + 1], buffer2[i2 + 2]);
    QR_Vector3DP v3b(buffer2[i3], buffer2[i3 + 1], buffer2[i3 + 2]);

    // create simple polygon interpolating both polygons values
    return new QR_SimplePolygon(v1a.Interpolate(v1b, factor),
                                v2a.Interpolate(v2b, factor),
                                v3a.Interpolate(v3b, factor));
}
//------------------------------------------------------------------------------
