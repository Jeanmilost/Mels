/******************************************************************************
 * ==> QR_MD2NormalsFile -----------------------------------------------------*
 ******************************************************************************
 * Description : MD2 pre-calculated normals table file                        *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#include "QR_MD2NormalsFile.h"

// std
#include <cstdio>
#include <sstream>

// interface
#include "QR_MD2Normals.h"

//------------------------------------------------------------------------------
// QR_MD2NormalsFile - c++ cross-platform
//------------------------------------------------------------------------------
QR_MD2NormalsFile::QR_MD2NormalsFile(const float& version, std::size_t normalCount)
{
    m_Version     = version;
    m_NormalCount = normalCount;
}
//------------------------------------------------------------------------------
QR_MD2NormalsFile::~QR_MD2NormalsFile()
{}
//------------------------------------------------------------------------------
bool QR_MD2NormalsFile::Save(const std::string& fileName, TMemo* pMemo)
{
    std::FILE* pFile = NULL;

    try
    {
        // is file name empty?
        if (fileName.empty())
        {
            LogLine("FAILED - file name is empty.", pMemo);
            return false;
        }

        // try to open file
        pFile = std::fopen(fileName.c_str(), "wb");

        // succeeded?
        if (!pFile)
        {
            LogLine("FAILED - could not open file " + fileName + " for save.", pMemo);
            return false;
        }

        LogLine("File " + fileName + " opened for save.", pMemo);

        // write file header
        std::fwrite(&m_Version,     sizeof(float),    1, pFile);
        std::fwrite(&m_NormalCount, sizeof(unsigned), 1, pFile);

        // log file header
        std::ostringstream sstr;
        sstr << "File version  - " << m_Version << "\r\n";
        sstr << "Normals count - " << m_NormalCount;
        LogLine(sstr.str(), pMemo);

        // iterate through normals
        for (std::size_t i = 0; i < m_NormalCount; ++i)
        {
            // write normal
            std::fwrite(&QR_MD2Normals::m_Table[i].m_X, sizeof(float), 1, pFile);
            std::fwrite(&QR_MD2Normals::m_Table[i].m_Y, sizeof(float), 1, pFile);
            std::fwrite(&QR_MD2Normals::m_Table[i].m_Z, sizeof(float), 1, pFile);

            // log normal
            std::ostringstream normSstr;
            normSstr << "Added normal  - [" << QR_MD2Normals::m_Table[i].m_X
                     << ", "                << QR_MD2Normals::m_Table[i].m_Y
                     << ", "                << QR_MD2Normals::m_Table[i].m_Z
                     << "]";
            LogLine(normSstr.str(), pMemo);
        }
    }
    __finally
    {
        // close file
        if (pFile)
            std::fclose(pFile);
    }

    LogLine("File " + fileName + " created successfully.", pMemo);

    return true;
}
//------------------------------------------------------------------------------
void __fastcall QR_MD2NormalsFile::LogLine(const std::string& message, TMemo* pMemo)
{
    // no message to log?
    if (message.empty())
        return;

    // no memo to log to?
    if (!pMemo)
        return;

    // log line
    pMemo->Lines->Add(message.c_str());
}
//------------------------------------------------------------------------------
