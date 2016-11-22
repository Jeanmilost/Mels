/******************************************************************************
 * ==> QR_FramedModelGroup ---------------------------------------------------*
 ******************************************************************************
 * Description : Framed model group, contains all items and functions needed  *
 *               to manage a complete model based on frames                   *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_FramedModelGroup.h"

//------------------------------------------------------------------------------
// QR_FramedModelGroup::IAnimation - c++ cross-platform
//------------------------------------------------------------------------------
QR_FramedModelGroup::IAnimation::IAnimation() :
    m_FrameIndex(0),
    m_InterpolationFrameIndex(0),
    m_InterpolationFactor(0.0),
    m_Loop(false)
{}
//------------------------------------------------------------------------------
QR_FramedModelGroup::IAnimation::IAnimation(QR_SizeT  frameIndex,
                                            QR_SizeT  interpolationFrameIndex,
                                            QR_Double interpolationFactor,
                                            bool      loop) :
    m_FrameIndex(frameIndex),
    m_InterpolationFrameIndex(interpolationFrameIndex),
    m_InterpolationFactor(interpolationFactor),
    m_Loop(loop)
{}
//------------------------------------------------------------------------------
QR_FramedModelGroup::IAnimation::~IAnimation()
{}
//------------------------------------------------------------------------------
// QR_FramedModelGroup - c++ cross-platform
//------------------------------------------------------------------------------
QR_FramedModelGroup::QR_FramedModelGroup() :
    QR_ModelGroup(),
    m_ForceLoop(true),
    m_fOnDrawItem(NULL),
    m_fOnAnimationEnd(NULL)
{}
//------------------------------------------------------------------------------
QR_FramedModelGroup::~QR_FramedModelGroup()
{}
//------------------------------------------------------------------------------
void QR_FramedModelGroup::ForceLoop(bool value)
{
    m_ForceLoop = value;
}
//------------------------------------------------------------------------------
void QR_FramedModelGroup::Set_OnDrawItem(ITfOnDrawItem fHandler)
{
    m_fOnDrawItem = fHandler;
}
//------------------------------------------------------------------------------
void QR_FramedModelGroup::Set_OnAnimationEnd(ITfOnAnimationEnd fHandler)
{
    m_fOnAnimationEnd = fHandler;
}
//------------------------------------------------------------------------------
QR_FramedModelGroup::IAnimation QR_FramedModelGroup::GetAnimation(const QR_Double& elapsedTime,
                                                                        QR_SizeT   fps,
                                                                        QR_SizeT   start,
                                                                        QR_SizeT   end,
                                                                        bool       loop,
                                                                        QR_SizeT&  frameIndex,
                                                                        QR_Double& interpolationFactor) const
{
    // no animation info?
    if (!fps || start == end)
        return IAnimation(start, start, 0.0, false);

    // calculate time interval between each frames
    const QR_Double timeInterval = (1000.0 / (QR_Double)fps);

    // calculate how many frames must be incremented since the last rendering
    interpolationFactor += (elapsedTime / timeInterval);

    // should increment one frame or more?
    if (interpolationFactor >= 1.0)
    {
        // calculate number of frames to increment
        const QR_SizeT frameCount = std::floor(interpolationFactor);

        // calculate interpolation factor (should always be between 0 and 1)
        interpolationFactor -= frameCount;

        // move frame index to next frame to show
        frameIndex += frameCount;
    }

    // not looping?
    if (!loop && !m_ForceLoop)
    {
        QR_SizeT nextFrameIndex;

        // last frame reached?
        if (frameIndex > end)
        {
            // show last frame
            frameIndex          = end;
            nextFrameIndex      = end;
            interpolationFactor = 0.0;

            // animation end reached?
            if (m_fOnAnimationEnd)
                m_fOnAnimationEnd();
        }
        else
        {
            // show next frame
            nextFrameIndex = frameIndex + 1;

            if (nextFrameIndex > end)
                nextFrameIndex = end;
        }

        return IAnimation(frameIndex, nextFrameIndex, interpolationFactor, loop);
    }

    // calculate next indexes to render
                   frameIndex     = ValidateIndex(frameIndex,     start, end);
    const QR_SizeT nextFrameIndex = ValidateIndex(frameIndex + 1, start, end);

    // return animation data
    return IAnimation(frameIndex, nextFrameIndex, interpolationFactor, loop);
}
//------------------------------------------------------------------------------
