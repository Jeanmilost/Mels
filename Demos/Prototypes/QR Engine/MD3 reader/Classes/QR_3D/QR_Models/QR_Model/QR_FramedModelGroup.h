/******************************************************************************
 * ==> QR_FramedModelGroup ---------------------------------------------------*
 ******************************************************************************
 * Description : Framed model group, contains all items and functions needed  *
 *               to manage a complete model based on frames                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_FramedModelGroupH
#define QR_FramedModelGroupH

// qr engine
#include "QR_Model.h"
#include "QR_ModelGroup.h"

/**
* Framed model group, contains all items and functions needed to manage a complete framed model
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_FramedModelGroup : public QR_ModelGroup
{
    public:
        /**
        * Called when model item should be drawn
        *@param pModel - model to draw
        *@param textures - textures belonging to model, in the order where they should be combined
        *@param matrix - model matrix
        *@param pMesh - mesh to draw
        *@param pNextMesh - next mesh to interpolate with, can be NULL
        *@param interpolationFactor - interpolation factor
        */
        #ifdef __CODEGEARC__
            typedef void (__closure *ITfOnDrawItem)(const QR_Model*     pModel,
                                                    const ITextures&    textures,
                                                    const QR_Matrix16P& matrix,
                                                    const QR_Mesh*      pMesh,
                                                    const QR_Mesh*      pNextMesh           = NULL,
                                                          QR_Double     interpolationFactor = 0.0);
        #else
            typedef void (*ITfOnDrawItem)(const QR_Model*     pModel,
                                          const ITextures&    textures,
                                          const QR_Matrix16P& matrix,
                                          const QR_Mesh*      pMesh,
                                          const QR_Mesh*      pNextMesh           = NULL,
                                                QR_Double     interpolationFactor = 0.0);
        #endif

        /**
        * Called when model animation ends
        */
        #ifdef __CODEGEARC__
            typedef void (__closure *ITfOnAnimationEnd)();
        #else
            typedef void (*ITfOnAnimationEnd)();
        #endif

        QR_FramedModelGroup();
        virtual ~QR_FramedModelGroup();

        /**
        * Sets force loop
        *@param value - if true, animation will always loop
        */
        virtual void ForceLoop(bool value);

        /**
        * Sets OnLoadTexture callback
        *@param fHandler - function handler
        */
        virtual void Set_OnDrawItem(ITfOnDrawItem fHandler);

        /**
        * Sets OnAnimationEnd callback
        *@param fHandler - function handler
        */
        virtual void Set_OnAnimationEnd(ITfOnAnimationEnd fHandler);

    protected:
        /**
        * Animation data structure
        */
        struct IAnimation
        {
            QR_SizeT  m_FrameIndex;
            QR_SizeT  m_InterpolationFrameIndex;
            QR_Double m_InterpolationFactor;
            bool      m_Loop;

            IAnimation();

            /**
            * Constructor
            *@param frameIndex - frame index to render
            *@param interpolationFrameIndex - frame index to use for interpolation
            *@param interpolationFactor - interpolation factor in percent (between 0.0f and 1.0f)
            *@param loop - if true, animation should loop at end
            */
            IAnimation(QR_SizeT  frameIndex,
                       QR_SizeT  interpolationFrameIndex,
                       QR_Double interpolationFactor,
                       bool      loop);

            virtual ~IAnimation();
        };

        bool              m_ForceLoop;
        ITfOnDrawItem     m_fOnDrawItem;
        ITfOnAnimationEnd m_fOnAnimationEnd;

        /**
        * Gets animation
        *@param elapsedTime - elapsed time since last frame was drawn
        *@param fps - number of frames per seconds
        *@param start - animation start index
        *@param end - animation end index
        *@aram loop - if true, animation should loop at end
        *@param[in, out] frameIndex - frame index, next frame index to draw when function ends
        *@param[in, out] interpolationFactor - interpolation factor between 2 frames
        *@return animation
        */
        virtual IAnimation GetAnimation(const QR_Double& elapsedTime,
                                              QR_SizeT   fps,
                                              QR_SizeT   start,
                                              QR_SizeT   end,
                                              bool       loop,
                                              QR_SizeT&  frameIndex,
                                              QR_Double& interpolationFactor) const;

        /**
        * Checks and returns an index that is always within the range delimited by start and end
        *@param index - index to check
        *@param start - range start
        *@param end - range end
        *@return an index that is always within the range delimited by start and end
        */
        virtual inline QR_SizeT ValidateIndex(QR_SizeT index, QR_SizeT start, QR_SizeT end) const;
};
//------------------------------------------------------------------------------
// QR_FramedModelGroup - c++ cross-platform
//------------------------------------------------------------------------------
QR_SizeT QR_FramedModelGroup::ValidateIndex(QR_SizeT index, QR_SizeT start, QR_SizeT end) const
{
    // is index out of bounds?
    if (index >= start && index <= end)
        return index;

    // calculate range
    const QR_SizeT range = (end - start) + 1;

    // calculate and return index within the range delimited by start and end
    return (start + ((index - start) % range));
}
//------------------------------------------------------------------------------

#endif
