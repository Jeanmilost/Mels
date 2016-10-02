/******************************************************************************
 * ==> QR_ModelCollision -----------------------------------------------------*
 ******************************************************************************
 * Description : Collision data to use with a model                           *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_ModelCollisionH
#define QR_ModelCollisionH

// qr engine
#include "QR_Types.h"
#include "QR_Vertex.h"
#include "QR_AABBTree.h"
#include "QR_Model.h"

/**
* Collision data to use with a model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_ModelCollision
{
    public:
        /**
        * Constructor
        */
        QR_ModelCollision();

        /**
        * Destructor
        */
        virtual ~QR_ModelCollision();

        /**
        * Gets linked model
        *@return linked model
        */
        virtual QR_Model* GetModel() const;

        /**
        * Sets model to link with
        *@param pModel - model to link with
        */
        virtual void SetModel(QR_Model* pModel);

        /**
        * Resolves collision
        *@param index - mesh index for which collision should be resolved
        *@param ray - ray to test against
        *@return true if ray is in collision with model, otherwise false
        */
        virtual bool Resolve(QR_SizeT index, const QR_RayP& ray) const;

        /**
        * Resolves collision
        *@param index - mesh index for which collision should be resolved
        *@param ray - ray to test against
        *@param[out] polygonList - polygon list that are in collision with ray
        *@return true if ray is in collision with model, otherwise false
        */
        virtual bool Resolve(QR_SizeT index, const QR_RayP& ray, QR_PolygonList& polygonList) const;

        /**
        * Gets polygons that can potentially be in collision with ray
        *@param index - mesh index for which collision should be resolved
        *@param ray - ray to test against
        *@param[out] polygonList - polygon list that can be in collision with ray
        *@return true on success, otherwise false
        */
        virtual bool GetPolygonsToCheck(      QR_SizeT        index,
                                        const QR_RayP&        ray,
                                              QR_PolygonList& polygonList) const;

    protected:
        /**
        * Collide item, contains a mesh, his polygons and an aabb tree
        */
        struct ICollideItem
        {
            QR_Mesh*       m_Mesh;
            QR_PolygonList m_PolygonList;
            QR_AABBTree*   m_pTree;

            /**
            * Constructor
            */
            ICollideItem();

            /**
            * Destructor
            */
            virtual ~ICollideItem();
        };

        typedef std::vector<ICollideItem*> ICollideFrames;

        QR_Model*      m_pModel;
        ICollideFrames m_Frames;

        /**
        * Adds a mesh for collision detection
        *@param pMesh - mesh to add
        *@return true on success, otherwise false
        *@note For preformance reasons, duplicates are not checked, so be careful when adding a mesh
        */
        virtual bool AddMesh(QR_Mesh* pMesh);
};

#endif // QR_ModelCollisionH
