/******************************************************************************
 * ==> QR_ModelCollision -----------------------------------------------------*
 ******************************************************************************
 * Description : Collision data to use with a model                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_ModelCollision.h"

// qr engine
#include "QR_STDTools.h"
#include "QR_3DCollisionHelper.h"
#include "QR_FramedModel.h"

//------------------------------------------------------------------------------
// QR_ModelCollision::ICollideItem - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelCollision::ICollideItem::ICollideItem() :
    m_pTree(NULL)
{}
//------------------------------------------------------------------------------
QR_ModelCollision::ICollideItem::~ICollideItem()
{
    // delete aabb tree
    if (m_pTree)
        delete m_pTree;

    // delete polygon list
    QR_STDTools::DelAndClear(m_PolygonList);
}
//------------------------------------------------------------------------------
// QR_ModelCollision - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelCollision::QR_ModelCollision() :
    m_pModel(NULL)
{}
//------------------------------------------------------------------------------
QR_ModelCollision::~QR_ModelCollision()
{
    // delete frames
    QR_STDTools::DelAndClear(m_Frames);
}
//------------------------------------------------------------------------------
QR_Model* QR_ModelCollision::GetModel() const
{
    return m_pModel;
}
//------------------------------------------------------------------------------
void QR_ModelCollision::SetModel(QR_Model* pModel)
{
    // something to do?
    if (m_pModel == pModel)
        return;

    m_pModel = pModel;

    // delete frames
    QR_STDTools::DelAndClear(m_Frames);

    // no new linked model?
    if (!m_pModel)
        return;

    // get model as framed model
    QR_FramedModel* pFramedModel = dynamic_cast<QR_FramedModel*>(m_pModel);

    // found it?
    if (pFramedModel)
    {
        // get mesh count
        const QR_SizeT meshCount = pFramedModel->GetMeshCount();

        // keep current vertex format
        const QR_Vertex::IEFormat vertexFormat = m_pModel->GetVertexFormat();

        try
        {
            // release vertex format, thus only position will be computed
            m_pModel->SetVertexFormat((QR_Vertex::IEFormat)0);

            // iterate through meshes to compute
            for (QR_SizeT i = 0; i < meshCount; ++i)
            {
                // get mesh from model
                std::auto_ptr<QR_Mesh> pMesh(new QR_Mesh());
                pFramedModel->GetMesh(i, *pMesh.get());

                // try to add it to collide list
                if (AddMesh(pMesh.get()))
                    pMesh.release();
            }
        }
        catch (...)
        {}

        // restore previous vertex format
        m_pModel->SetVertexFormat(vertexFormat);
    }
}
//------------------------------------------------------------------------------
bool QR_ModelCollision::Resolve(QR_SizeT index, const QR_RayP& ray) const
{
    QR_PolygonList polygonList;
    return Resolve(index, ray, polygonList);
}
//------------------------------------------------------------------------------
bool QR_ModelCollision::Resolve(      QR_SizeT        index,
                                const QR_RayP&        ray,
                                      QR_PolygonList& polygonList) const
{
    QR_PolygonList polygonsToCheck;

    // resolve aligned-axis bounding box tree
    GetPolygonsToCheck(index, ray, polygonsToCheck);

    // found polygons to check?
    if (!polygonsToCheck.size())
        return false;

    bool collision = false;

    // iterate through polygons to check
    for (QR_PolygonList::iterator it = polygonsToCheck.begin(); it != polygonsToCheck.end(); ++it)
        // is polygon intersecting ray?
        if (QR_3DCollisionHelper::GetTriRayCollision(ray, *(*it)))
        {
            // add polygon to resulting list
            polygonList.push_back(*it);
            collision = true;
        }

    return collision;
}
//------------------------------------------------------------------------------
bool QR_ModelCollision::GetPolygonsToCheck(      QR_SizeT        index,
                                           const QR_RayP&        ray,
                                                 QR_PolygonList& polygonList) const
{
    // get mesh count
    const QR_SizeT meshCount = m_Frames.size();

    // is index out of bounds?
    if (index >= meshCount)
        return false;

    // is mesh correctly initialized?
    if (!m_Frames[index] || !m_Frames[index]->m_pTree)
        return false;

    // resolve tree matching with current mesh
    return m_Frames[index]->m_pTree->Resolve(ray, polygonList);
}
//------------------------------------------------------------------------------
bool QR_ModelCollision::AddMesh(QR_Mesh* pMesh)
{
    // no mesh to add?
    if (!pMesh)
        return false;

    // create and populate new collide item
    std::auto_ptr<ICollideItem> pItem(new ICollideItem());
    pItem->m_Mesh = pMesh;

    // get vertices count
    const QR_SizeT vertexCount = pMesh->size();

    // iterate through vertices to compute
    for (QR_SizeT i = 0; i < vertexCount; ++i)
        // get collide polygons
        QR_3DCollisionHelper::GetPolygons((*pMesh)[i],
                                          QR_3DCollisionHelper::IE_PT_Indexed,
                                          pItem->m_PolygonList);

    // compute aligned-axis bounding box tree from polygons
    std::auto_ptr<QR_AABBTree> pAABBTree(new QR_AABBTree(pItem->m_PolygonList));
    pItem->m_pTree = pAABBTree.get();
    pAABBTree.release();

    m_Frames.push_back(pItem.get());
    pItem.release();

    return true;
}
//------------------------------------------------------------------------------
