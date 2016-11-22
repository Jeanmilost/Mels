/******************************************************************************
 * ==> QR_ModelGroup ---------------------------------------------------------*
 ******************************************************************************
 * Description : Model group, contains all items and functions needed to      *
 *               manage a complete model                                      *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_ModelGroup.h"

// qr engine
#include "QR_Exception.h"
#include "QR_STDTools.h"

//------------------------------------------------------------------------------
// QR_ModelGroup::ITexture - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelGroup::ITexture::ITexture() :
    m_Index(0),
    m_WrapMode(IE_WM_REPEAT)
{}
//------------------------------------------------------------------------------
QR_ModelGroup::ITexture::~ITexture()
{}
//------------------------------------------------------------------------------
// QR_ModelGroup::IMemoryDir - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelGroup::IMemoryDir::IMemoryDir(bool deleteOnDestroy) :
    m_DeleteOnDestroy(deleteOnDestroy)
{}
//------------------------------------------------------------------------------
QR_ModelGroup::IMemoryDir::~IMemoryDir()
{
    // do delete file on destroy?
    if (m_DeleteOnDestroy)
        // clear memory
        QR_STDTools::DelAndClear(m_Files);
}
//------------------------------------------------------------------------------
bool QR_ModelGroup::IMemoryDir::AddFile(const std::wstring& fileName,
                                        QR_MemoryBuffer*    pBuffer,
                                        bool                overwrite)
{
    // no buffer to add?
    if (!pBuffer)
        return false;

    // search for file
    IMemoryFileDictionary::iterator it = m_Files.find(fileName);

    // file exists?
    if (it != m_Files.end())
    {
        // file cannot be overwritten?
        if (!overwrite)
            return false;

        // do delete file content?
        if (m_DeleteOnDestroy)
        {
            delete it->second;
            m_Files.erase(it);
        }
    }

    // add file to dir list
    m_Files[fileName] = pBuffer;
    return true;
}
//------------------------------------------------------------------------------
QR_MemoryBuffer* QR_ModelGroup::IMemoryDir::GetFile(const std::wstring& fileName)
{
    // search for file
    IMemoryFileDictionary::iterator it = m_Files.find(fileName);

    // found it?
    if (it == m_Files.end())
        return NULL;

    return it->second;
}
//------------------------------------------------------------------------------
bool QR_ModelGroup::IMemoryDir::FileExists(const std::wstring& fileName) const
{
    return (m_Files.find(fileName) != m_Files.end());
}
//------------------------------------------------------------------------------
// QR_ModelGroup::IRotation - c++ cross-platform
//------------------------------------------------------------------------------
QR_ModelGroup::IRotation::IRotation() : m_Angle(0.0f)
{}
//------------------------------------------------------------------------------
QR_ModelGroup::IRotation::IRotation(const M_Precision& angle, const QR_Vector3DP& axis) :
    m_Angle(angle),
    m_Axis(axis)
{}
//------------------------------------------------------------------------------
QR_ModelGroup::IRotation::~IRotation()
{}
//---------------------------------------------------------------------------
// QR_ModelGroup - c++ cross-platform
//---------------------------------------------------------------------------
QR_ModelGroup::QR_ModelGroup() :
    m_Scaling(QR_Vector3DP(1.0f, 1.0f, 1.0f)),
    m_Translation(QR_Vector3DP(1.0f, 1.0f, 1.0f)),
    m_RotationX(IRotation(0.0f, QR_Vector3DP(1.0f, 0.0f, 0.0f))),
    m_RotationY(IRotation(0.0f, QR_Vector3DP(0.0f, 1.0f, 0.0f))),
    m_RotationZ(IRotation(0.0f, QR_Vector3DP(0.0f, 0.0f, 1.0f))),
    m_CombinationType(IE_CT_Scale_Rotate_Translate),
    m_pInitialMatrix(NULL),
    m_fOnLoadTexture(NULL)
{}
//---------------------------------------------------------------------------
QR_ModelGroup::~QR_ModelGroup()
{
    if (m_pInitialMatrix)
        delete m_pInitialMatrix;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_ModelGroup::GetTranslation() const
{
    return m_Translation;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetTranslation(const QR_Vector3DP& translation)
{
    m_Translation = translation;
}
//------------------------------------------------------------------------------
M_Precision QR_ModelGroup::GetRotationX() const
{
    return m_RotationX.m_Angle;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetRotationX(const M_Precision& angle)
{
    m_RotationX.m_Angle = angle;
}
//------------------------------------------------------------------------------
M_Precision QR_ModelGroup::GetRotationY() const
{
    return m_RotationY.m_Angle;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetRotationY(const M_Precision& angle)
{
    m_RotationY.m_Angle = angle;
}
//------------------------------------------------------------------------------
M_Precision QR_ModelGroup::GetRotationZ() const
{
    return m_RotationZ.m_Angle;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetRotationZ(const M_Precision& angle)
{
    m_RotationZ.m_Angle = angle;
}
//------------------------------------------------------------------------------
QR_Vector3DP QR_ModelGroup::GetScaling() const
{
    return m_Scaling;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetScaling(const QR_Vector3DP& scaling)
{
    m_Scaling = scaling;
}
//------------------------------------------------------------------------------
QR_ModelGroup::IECombinationType QR_ModelGroup::GetMatrixCombinationType() const
{
    return m_CombinationType;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetMatrixCombinationType(IECombinationType type)
{
    m_CombinationType = type;
}
//------------------------------------------------------------------------------
QR_Matrix16P* QR_ModelGroup::GetInitialMatrix() const
{
    return m_pInitialMatrix;
}
//------------------------------------------------------------------------------
void QR_ModelGroup::SetInitialMatrix(const QR_Matrix16P* pMatrix)
{
    // previous initial matrix was set?
    if (m_pInitialMatrix)
        delete m_pInitialMatrix;

    // no new initial matrix to set?
    if (!pMatrix)
    {
        m_pInitialMatrix = NULL;
        return;
    }

    // copy initial matrix
    m_pInitialMatrix = new QR_Matrix16P(*pMatrix);
}
//------------------------------------------------------------------------------
void QR_ModelGroup::Set_OnLoadTexture(ITfOnLoadTexture fHandler)
{
    m_fOnLoadTexture = fHandler;
}
//------------------------------------------------------------------------------
QR_Matrix16P QR_ModelGroup::GetMatrix() const
{
    // initialize matrix
    QR_Matrix16P scaleMatrix     = QR_Matrix16P::Identity();
    QR_Matrix16P rotateXMatrix   = QR_Matrix16P::Identity();
    QR_Matrix16P rotateYMatrix   = QR_Matrix16P::Identity();
    QR_Matrix16P rotateZMatrix   = QR_Matrix16P::Identity();
    QR_Matrix16P translateMatrix = QR_Matrix16P::Identity();
    QR_Matrix16P modelMatrix     = m_pInitialMatrix ? *m_pInitialMatrix : QR_Matrix16P::Identity();

    // build scaling, rotation and translation matrix
    scaleMatrix.Scale(m_Scaling);
    rotateXMatrix.Rotate(m_RotationX.m_Angle, m_RotationX.m_Axis);
    rotateYMatrix.Rotate(m_RotationY.m_Angle, m_RotationY.m_Axis);
    rotateZMatrix.Rotate(m_RotationZ.m_Angle, m_RotationZ.m_Axis);
    translateMatrix.Translate(m_Translation);

    // build model matrix
    switch (m_CombinationType)
    {
        case IE_CT_Scale_Rotate_Translate:
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            break;

        case IE_CT_Scale_Translate_Rotate:
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            break;

        case IE_CT_Rotate_Translate_Scale:
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            break;

        case IE_CT_Rotate_Scale_Translate:
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            break;

        case IE_CT_Translate_Rotate_Scale:
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            break;

        case IE_CT_Translate_Scale_Rotate:
            modelMatrix = modelMatrix.Multiply(translateMatrix);
            modelMatrix = modelMatrix.Multiply(scaleMatrix);
            modelMatrix = modelMatrix.Multiply(rotateXMatrix);
            modelMatrix = modelMatrix.Multiply(rotateYMatrix);
            modelMatrix = modelMatrix.Multiply(rotateZMatrix);
            break;

        default:
            M_THROW_EXCEPTION("Unknown model matrix combination type");
    }

    return modelMatrix;
}
//---------------------------------------------------------------------------
