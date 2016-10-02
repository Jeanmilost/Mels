/******************************************************************************
 * ==> QR_MD3AnimCfgFile -----------------------------------------------------*
 ******************************************************************************
 * Description : MD3 animation configuration file parser                      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD3AnimCfgFileH
#define QR_MD3AnimCfgFileH

// qr engine
#include "QR_Script.h"

/**
* MD3 animation configuration file parser
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD3AnimCfgFile : public QR_Script
{
    public:
        /**
        * Model gender
        */
        enum IEGender
        {
            IE_GN_Unknown = 0,
            IE_GN_Male,
            IE_GN_Female,
        };

        /**
        * Standard MD3 animations gestures enumeration
        *@note These values should never be reordered, as they match with the order in file
        */
        enum IEGesture
        {
            IE_AG_BOTH_DEATH1 = 0,
            IE_AG_BOTH_DEAD1,
            IE_AG_BOTH_DEATH2,
            IE_AG_BOTH_DEAD2,
            IE_AG_BOTH_DEATH3,
            IE_AG_BOTH_DEAD3,
            IE_AG_TORSO_GESTURE,
            IE_AG_TORSO_ATTACK,
            IE_AG_TORSO_ATTACK2,
            IE_AG_TORSO_DROP,
            IE_AG_TORSO_RAISE,
            IE_AG_TORSO_STAND,
            IE_AG_TORSO_STAND2,
            IE_AG_LEGS_WALK_CROUCHING,
            IE_AG_LEGS_WALK,
            IE_AG_LEGS_RUN,
            IE_AG_LEGS_BACK,
            IE_AG_LEGS_SWIM,
            IE_AG_LEGS_JUMP,
            IE_AG_LEGS_LAND,
            IE_AG_LEGS_JUMP_BACK,
            IE_AG_LEGS_LAND_BACK,
            IE_AG_LEGS_IDLE,
            IE_AG_LEGS_IDLE_CROUCHING,
            IE_AG_LEGS_TURN,
            IE_AG_MAX_ANIMATIONS,
        };

        /**
        * Foot step mode
        */
        enum IEFootStep
        {
            IE_FS_Unknown,
            IE_FS_Boot,
        };

        /**
        * Structure containing headoffset instruction, don't know what exactly means
        */
        struct IHeadOffset
        {
            int m_UnknownOffset1;
            int m_UnknownOffset2;
            int m_UnknownOffset3;

            IHeadOffset();
            virtual ~IHeadOffset();
        };

        /**
        * Structure containing footsteps instruction, don't know what exactly means
        */
        struct IFootSteps
        {
            IEFootStep m_Mode;

            IFootSteps();
            virtual ~IFootSteps();
        };

        /**
        * Animation item, contains important animation info, as e.g. start and end frames, FPS, ...
        */
        struct IItem
        {
            IEGesture m_Gesture;
            QR_SizeT  m_StartFrame;
            QR_SizeT  m_FrameCount;
            QR_SizeT  m_LoopingFrames;
            QR_SizeT  m_FramesPerSecond;

            IItem();
            virtual ~IItem();
        };

        typedef std::vector<IItem*> IItems;

        QR_MD3AnimCfgFile();
        virtual ~QR_MD3AnimCfgFile();

        /**
        * Clears animation configuration
        */
        virtual void Clear();

        /**
        * Gets model gender
        *@return model gender
        */
        virtual IEGender GetGender() const;

        /**
        * Gets header offset
        *@return header offset
        */
        virtual IHeadOffset GetHeadOffset() const;

        /**
        * Gets foot steps
        *@return foot steps
        */
        virtual IFootSteps GetFootStep() const;

        /**
        * Gets animation item at index
        *@return animation item at index, NULL if not found or on error
        */
        virtual IItem* GetItem(QR_SizeT index) const;

        /**
        * Gets animation item count
        *@return animation item count
        */
        virtual QR_SizeT GetItemCount() const;

        /**
        * Converts animation gesture to string
        *@param gesture - animation gesture to convert
        *@return converted animation gesture as string
        */
        static std::string GestureToStr(IEGesture gesture);

    protected:
        IEGender    m_Gender;
        IHeadOffset m_HeadOffset;
        IFootSteps  m_FootSteps;
        IItems      m_Items;
        QR_SizeT    m_Column;
        QR_SizeT    m_StartLine;
        QR_SizeT    m_CurLine;
        bool        m_ReadGender;
        bool        m_ReadHeadOffset;
        bool        m_ReadFootSteps;
        bool        m_LongComment;

        /**
        * Called when script line should be parsed
        *@param line - line to parse
        *@param linbeNb - line number
        *@return ture on success, otherwise false
        */
        virtual bool OnParseLine(const std::string& line, QR_SizeT lineNb);

        /**
        * Parses a word found in script line
        *@param word - word to parse
        *@param lineNb - current parsing line number
        *@return true on success, otherwise false
        */
        virtual bool ParseWord(const std::string& word, QR_SizeT lineNb);
};

#endif
