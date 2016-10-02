/******************************************************************************
 * ==> QR_MD3Animation -------------------------------------------------------*
 ******************************************************************************
 * Description : Animation class for MD3 models                               *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3AnimationH
#define QR_MD3AnimationH

// std
#include <string>
#include <vector>
#include <map>

// qr engine
#include "QR_Types.h"
#include "QR_Vertex.h"
#include "QR_MD3Common.h"
#include "QR_MD3.h"

/**
* Animation class for MD3 models
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3Animation
{
    public:
        /**
        * Animation data structure
        */
        struct IAnimation
        {
            QR_SizeT  m_FrameIndex;
            QR_SizeT  m_InterpolationFrameIndex;
            QR_Double m_InterpolationFactor;

            IAnimation();

            /**
            * Constructor
            *@param frameIndex - frame index to draw
            *@param interpolationFrameIndex - frame index to use for interpolation
            *@param interpolationFactor - interpolation factor in percent (between 0.0f and 1.0f)
            */
            IAnimation(QR_SizeT frameIndex, QR_SizeT interpolationFrameIndex,
                    const QR_Double& interpolationFactor);

            virtual ~IAnimation();
        };

        typedef std::vector<QR_Mesh*>           IFrames;
        typedef std::map<std::string, IFrames*> IFrameDictionary;

        /**
        * Constructor
        *@param pModel - MD3 model to animate
        *@param useCache - if true, cache usage is enabled (should be false if GetMesh() isn't used)
        */
        QR_MD3Animation(QR_MD3* pModel, bool useCache = false);

        virtual ~QR_MD3Animation();

        /**
        * Enables or disables cache
        *@param value - if true, cache is enabled, otherwise disabled
        */
        virtual void SetUseCache(bool value);

        /**
        * Adds animation
        *@param name - animation name
        *@param start - frame index at which animation starts in the MD3 model
        *@param end - frame index at which animation ends in the MD3 model
        *@return true on success, otherwise false
        */
        virtual bool Add(const std::string& name, QR_SizeT start, QR_SizeT end);

        /**
        * Deletes animation
        *@param name - animation name to delete
        */
        virtual void Delete(const std::string& name);

        /**
        * Sets animation to use
        *@param name - animation name
        *@return true on success, otherwise false
        */
        virtual bool Set(const std::string& name);

        /**
        * Gets current running animation name
        *@return animation name, empty string if not found or on error
        */
        virtual std::string GetRunningAnimationName() const;

        /**
        * Gets current running animation start index
        *@return animation start index
        */
        virtual QR_SizeT GetRunningAnimationStartIndex() const;

        /**
        * Gets current running animation end index
        *@return animation end index
        */
        virtual QR_SizeT GetRunningAnimationEndIndex() const;

        /**
        * Gets animation
        *@param elapsedTime - elapsed time since last frame was drawn
        *@param fps - number of frames per seconds
        *@return animation
        */
        virtual IAnimation GetAnimation(const QR_Double& elapsedTime, QR_SizeT fps);

        /**
        * Gets mesh
        *@param elapsedTime - elapsed time since last frame was drawn
        *@param fps - number of frames per seconds
        *@param[out] mesh - mesh to draw
        *@param update - if true, resulting mesh vector is already initialized and should only be updated
        *@return true on success, otherwise false
        *@note For performances reasons, this function should not be used since OpenGL 2.0. Instead,
        *      mesh should be generated once from model and then passed to vertex shader, and
        *      interpolation should be done inside the vertex shader
        */
        virtual bool GetMesh(const QR_Double& elapsedTime,
                                   QR_SizeT   fps,
                                   QR_Mesh&   mesh);

        /**
        * Gets MD3 frames contained between start and end indexes (to connect with a shader)
        *@param start - start mesh index
        *@param end - end mesh index
        *@param[out] frames - model frames
        *@return true on success, otherwise false
        *@note WARNING, the returned vertex buffer structure differs as others.
        *      Here is the vertex buffer description:
        *      [1]x [2]y [3]z [4]ix [5]iy [6]iz [7]nx [8]ny [9]nz [10]inx [11]iny [12]inz [13]tu [14]tv [15]r [16]g [17]b [18]a
        *      where:
        *      x/y/z    - vertex coordinates
        *      ix/iy/iz - vertex coordinates to interpolate with
        *      nx/ny/nz - vertex normal (if includeNormal is activated)
        *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
        *      r/g/b/a  - vertex color(if includeColor is activated)
        *@note Last frame will be interpolated with first
        */
        virtual bool GetFrames(QR_SizeT start, QR_SizeT end, IFrames& frames) const;

        /**
        * Gets all MD3 frames (to connect with a shader)
        *@param start - start mesh index
        *@param end - end mesh index
        *@param[out] frames - model frames
        *@return true on success, otherwise false
        *@note WARNING, the returned vertex buffer structure differs as others.
        *      Here is the vertex buffer description:
        *      [1]x [2]y [3]z [4]ix [5]iy [6]iz [7]nx [8]ny [9]nz [10]inx [11]iny [12]inz [13]tu [14]tv [15]r [16]g [17]b [18]a
        *      where:
        *      x/y/z    - vertex coordinates
        *      ix/iy/iz - vertex coordinates to interpolate with
        *      nx/ny/nz - vertex normal (if includeNormal is activated)
        *      tu/tv    - vertex texture coordinates(if includeTexture is activated)
        *      r/g/b/a  - vertex color(if includeColor is activated)
        */
        virtual bool GetFrames(IFrameDictionary& frameDict) const;

    private:
        /**
        * Animation info structure
        */
        struct IInfo
        {
            QR_SizeT m_Start;
            QR_SizeT m_End;

            IInfo();

            /**
            * Constructor
            *@param start - start frame index
            *@param end - end frame index
            */
            IInfo(QR_SizeT start, QR_SizeT end);

            virtual ~IInfo();
        };

        typedef std::map<std::string, IInfo*> IInfoDictionary;
        typedef std::vector<QR_Mesh>          ICache;

        QR_MD3*         m_pModel;
        IInfo*          m_pInfo;
        IInfoDictionary m_InfoDictionary;
        ICache          m_Cache;
        QR_SizeT        m_FrameIndex;
        QR_Double       m_InterpolationFactor;
        std::string     m_CurAnimName;
};

#endif // QR_MD3AnimationH
