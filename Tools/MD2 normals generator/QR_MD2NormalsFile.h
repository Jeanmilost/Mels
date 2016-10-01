/******************************************************************************
 * ==> QR_MD2NormalsFile -----------------------------------------------------*
 ******************************************************************************
 * Description : MD2 pre-calculated normals table file                        *
 * Developer   : Jean-Milost Reymond                                          *
 ******************************************************************************/

#ifndef QR_MD2NormalsFileH
#define QR_MD2NormalsFileH

// vcl
#include <Vcl.StdCtrls.hpp>

// std
#include <string>

/**
* Class to create MD2 pre-calculated normals binary file
*@note This class is cross-platform
*@author Jean-Milost Reymond
*/
class QR_MD2NormalsFile
{
  public:
    /**
    * Constructor
    *@param version - file version
    *@param normalCount - normal count
    */
    QR_MD2NormalsFile(const float& version, std::size_t normalCount);

    /**
    * Destructor
    */
    virtual ~QR_MD2NormalsFile();

    /**
    * Saves file
    *@param fileName - file name
    *@aram pMemo - log memo
    *@return true on success, otherwise false
    */
    bool Save(const std::string& fileName, TMemo* pMemo);

  private:
    float       m_Version;
    std::size_t m_NormalCount;

    /**
    * Logs a line
    *@param message - message to log
    *@param pMemo - memo to log line to
    */
    void __fastcall LogLine(const std::string& message, TMemo* pMemo);
};

#endif // QR_MD2NormalsFileH
