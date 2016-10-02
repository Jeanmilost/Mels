/******************************************************************************
 * ==> QR_MD3AnimCfgFile -----------------------------------------------------*
 ******************************************************************************
 * Description : MD3 animation configuration file parser                      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD3AnimCfgFile.h"

// qr engine
#include "QR_Exception.h"
#include "QR_STDTools.h"
#include "QR_StringTools.h"

//------------------------------------------------------------------------------
// QR_MD3AnimCfgFile::IItem - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IItem::IItem() :
    m_StartFrame(0),
    m_FrameCount(0),
    m_LoopingFrames(0),
    m_FramesPerSecond(0)
{}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IItem::~IItem()
{}
//------------------------------------------------------------------------------
// QR_MD3AnimCfgFile::IHeadOffset - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IHeadOffset::IHeadOffset() :
    m_UnknownOffset1(0),
    m_UnknownOffset2(0),
    m_UnknownOffset3(0)
{}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IHeadOffset::~IHeadOffset()
{}
//------------------------------------------------------------------------------
// QR_MD3AnimCfgFile::IFootSteps - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IFootSteps::IFootSteps() :
    m_Mode(IE_FS_Unknown)
{}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IFootSteps::~IFootSteps()
{}
//------------------------------------------------------------------------------
// QR_MD3AnimCfgFile - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::QR_MD3AnimCfgFile() :
    QR_Script(),
    m_Gender(IE_GN_Unknown),
    m_Column(0),
    m_StartLine(0),
    m_CurLine(0),
    m_ReadGender(false),
    m_ReadHeadOffset(false),
    m_ReadFootSteps(false),
    m_LongComment(false)
{}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::~QR_MD3AnimCfgFile()
{
    // clear memory
    QR_STDTools::DelAndClear(m_Items);
}
//------------------------------------------------------------------------------
void QR_MD3AnimCfgFile::Clear()
{
    // clear memory
    QR_STDTools::DelAndClear(m_Items);

    // reset values
    m_Gender         = IE_GN_Unknown;
    m_HeadOffset     = IHeadOffset();
    m_FootSteps      = IFootSteps();
    m_Column         = 0;
    m_StartLine      = 0;
    m_CurLine        = 0;
    m_ReadGender     = false;
    m_ReadHeadOffset = false;
    m_ReadFootSteps  = false;
    m_LongComment    = false;
}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IEGender QR_MD3AnimCfgFile::GetGender() const
{
    return m_Gender;
}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IHeadOffset QR_MD3AnimCfgFile::GetHeadOffset() const
{
    return m_HeadOffset;
}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IFootSteps QR_MD3AnimCfgFile::GetFootStep() const
{
    return m_FootSteps;
}
//------------------------------------------------------------------------------
QR_MD3AnimCfgFile::IItem* QR_MD3AnimCfgFile::GetItem(QR_SizeT index) const
{
    if (index >= m_Items.size())
        return NULL;

    return m_Items[index];
}
//------------------------------------------------------------------------------
QR_SizeT QR_MD3AnimCfgFile::GetItemCount() const
{
    return m_Items.size();
}
//------------------------------------------------------------------------------
std::string QR_MD3AnimCfgFile::GestureToStr(IEGesture gesture)
{
    switch (gesture)
    {
        case IE_AG_BOTH_DEATH1:         return "IE_AG_BOTH_DEATH1";
        case IE_AG_BOTH_DEAD1:          return "IE_AG_BOTH_DEAD1";
        case IE_AG_BOTH_DEATH2:         return "IE_AG_BOTH_DEATH2";
        case IE_AG_BOTH_DEAD2:          return "IE_AG_BOTH_DEAD2";
        case IE_AG_BOTH_DEATH3:         return "IE_AG_BOTH_DEATH3";
        case IE_AG_BOTH_DEAD3:          return "IE_AG_BOTH_DEAD3";
        case IE_AG_TORSO_GESTURE:       return "IE_AG_TORSO_GESTURE";
        case IE_AG_TORSO_ATTACK:        return "IE_AG_TORSO_ATTACK";
        case IE_AG_TORSO_ATTACK2:       return "IE_AG_TORSO_ATTACK2";
        case IE_AG_TORSO_DROP:          return "IE_AG_TORSO_DROP";
        case IE_AG_TORSO_RAISE:         return "IE_AG_TORSO_RAISE";
        case IE_AG_TORSO_STAND:         return "IE_AG_TORSO_STAND";
        case IE_AG_TORSO_STAND2:        return "IE_AG_TORSO_STAND2";
        case IE_AG_LEGS_WALK_CROUCHING: return "IE_AG_LEGS_WALK_CROUCHING";
        case IE_AG_LEGS_WALK:           return "IE_AG_LEGS_WALK";
        case IE_AG_LEGS_RUN:            return "IE_AG_LEGS_RUN";
        case IE_AG_LEGS_BACK:           return "IE_AG_LEGS_BACK";
        case IE_AG_LEGS_SWIM:           return "IE_AG_LEGS_SWIM";
        case IE_AG_LEGS_JUMP:           return "IE_AG_LEGS_JUMP";
        case IE_AG_LEGS_LAND:           return "IE_AG_LEGS_LAND";
        case IE_AG_LEGS_JUMP_BACK:      return "IE_AG_LEGS_JUMP_BACK";
        case IE_AG_LEGS_LAND_BACK:      return "IE_AG_LEGS_LAND_BACK";
        case IE_AG_LEGS_IDLE:           return "IE_AG_LEGS_IDLE";
        case IE_AG_LEGS_IDLE_CROUCHING: return "IE_AG_LEGS_IDLE_CROUCHING";
        case IE_AG_LEGS_TURN:           return "IE_AG_LEGS_TURN";
        case IE_AG_MAX_ANIMATIONS:      return "IE_AG_MAX_ANIMATIONS";
        default:                        M_THROW_EXCEPTION("Unknown animation type");
    }
}
//------------------------------------------------------------------------------
bool QR_MD3AnimCfgFile::OnParseLine(const std::string& line, QR_SizeT lineNb)
{
    m_Column         = 0;
    m_ReadGender     = false;
    m_ReadHeadOffset = false;
    m_ReadFootSteps  = false;

    // no line to parse?
    if (line.empty())
        return true;

    std::string data;

    // search for comment marker
    QR_SizeT commentPos = line.find("//", 0);

    // found it?
    if (commentPos == line.npos)
        // no, parse entire line
        data = line;
    else
        // yes, parse only the uncommented line part
        data = QR_StringTools::Trim(line.substr(0, commentPos));

    std::string word;

    // iterate through line chars
    for (QR_SizeT i = 0; i < data.length(); ++i)
        // search for char
        switch (data[i])
        {
            case '/':
            case '*':
                // found a long comment (i.e. comment between /* and */) start or end mark?
                if ((i + 1) < data.length())
                    if (data[i] == '/' && data[i + 1] == '*')
                    {
                        m_LongComment = true;
                        ++i;
                    }
                    else
                    if (data[i] == '*' && data[i + 1] == '/')
                    {
                        m_LongComment = false;
                        ++i;
                    }

                continue;

            case ' ':
            case '\t':
                // skip all chars inside a long comment
                if (m_LongComment)
                    continue;

                // found word to parse?
                if (!word.empty())
                {
                    // parse it
                    if (!ParseWord(word, lineNb))
                        return false;

                    // clear parsed word to read next
                    word.clear();
                }

                // skip all remaining spaces
                while ((i + 1) < data.length() && (data[i + 1] == ' ' || data[i + 1] == '\t'))
                    ++i;

                continue;

            default:
                // skip all chars inside a long comment
                if (m_LongComment)
                    continue;

                // add char to word
                word += data[i];
                continue;
        }

    // skip all chars inside a long comment
    if (m_LongComment)
        return true;

    // last word to parse?
    if (!word.empty())
        // parse it
        return ParseWord(word, lineNb);

    return true;
}
//------------------------------------------------------------------------------
bool QR_MD3AnimCfgFile::ParseWord(const std::string& word, QR_SizeT lineNb)
{
    // search word meaning
    if (word == "sex")
        // gender indicator
        m_ReadGender = true;
    else
    if (m_ReadGender)
    {
        // if gender is currently reading, m means male, and f means female. Another word is an error
        if (word == "m")
            m_Gender = IE_GN_Male;
        else
        if (word == "f")
            m_Gender = IE_GN_Female;
        else
            return false;
    }
    else
    if (word == "headoffset")
        // head offset indicator
        m_ReadHeadOffset = true;
    else
    if (m_ReadHeadOffset)
    {
        // all the words following the head offset indicator should be numeric values
        for (QR_SizeT i = 0; i < word.length(); ++i)
            if (!QR_StringTools::IsNumeric(word[i], false))
                return false;

        // search for head offset value to set
        switch (m_Column)
        {
            case 0:  m_HeadOffset.m_UnknownOffset1 = std::atoi(word.c_str()); break;
            case 1:  m_HeadOffset.m_UnknownOffset2 = std::atoi(word.c_str()); break;
            case 2:  m_HeadOffset.m_UnknownOffset3 = std::atoi(word.c_str()); break;
            default: return false;
        }

        ++m_Column;
    }
    else
    if (word == "footsteps")
        // foot steps indicator
        m_ReadFootSteps = true;
    else
    if (m_ReadFootSteps)
    {
        if (word == "boot")
            m_FootSteps.m_Mode = IE_FS_Boot;
    }
    else
    {
        // by default, each line contains 4 numeric values, that describes the animation
        for (QR_SizeT i = 0; i < word.length(); ++i)
            if (!QR_StringTools::IsNumeric(word[i], false))
                return false;

        // first item to parse?
        if (!m_Items.size())
        {
            // store the start line, it will be used later to find the animation type
            m_StartLine = lineNb;
            m_CurLine   = lineNb;
        }

        // first animation value?
        if (!m_Column)
        {
            // get animation gesture (each line pos in file matchs with the IEGesture enumerator)
            const int gesture = (m_CurLine - m_StartLine);

            // invalid type?
            if (gesture >= IE_AG_MAX_ANIMATIONS)
                return false;

            // create and populate new item, and add it to list
            std::auto_ptr<IItem> pNewItem(new IItem());
            pNewItem->m_Gesture = (IEGesture)gesture;
            m_Items.push_back(pNewItem.get());
            pNewItem.release();

            ++m_CurLine;
        }

        // get animation item to populate
        IItem* pItem = m_Items.back();

        // search for animation item value to set
        switch (m_Column)
        {
            case 0:  pItem->m_StartFrame      = std::atoi(word.c_str()); break;
            case 1:  pItem->m_FrameCount      = std::atoi(word.c_str()); break;
            case 2:  pItem->m_LoopingFrames   = std::atoi(word.c_str()); break;
            case 3:  pItem->m_FramesPerSecond = std::atoi(word.c_str()); break;
            default: return false;
        }

        ++m_Column;
    }

    return true;
}
//------------------------------------------------------------------------------
