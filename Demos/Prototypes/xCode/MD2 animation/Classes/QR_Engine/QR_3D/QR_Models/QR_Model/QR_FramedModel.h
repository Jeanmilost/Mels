/******************************************************************************
 * ==> QR_FramedModel --------------------------------------------------------*
 ******************************************************************************
 * Description : Base class for models based on frames, as e.g. MD2, MD3, ... *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_FramedModelH
#define QR_FramedModelH

// qr engine
#include "QR_Model.h"

/**
* Base class for models based on frames, as e.g. MD2, MD3, ...
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_FramedModel : public QR_Model
{
    public:
        QR_FramedModel();
        virtual ~QR_FramedModel();

        /**
        * Gets mesh
        *@param index - mesh index to get
        *@param[out] mesh - mesh
        *@return 0 on success, otherwise warning (positive value) or error (negative value) code
        */
        virtual QR_Int32 GetMesh(QR_SizeT index, QR_Mesh& mesh) const = 0;

        /**
        * Gets mesh count
        *@return mesh count
        */
        virtual QR_SizeT GetMeshCount() const = 0;
};

#endif
