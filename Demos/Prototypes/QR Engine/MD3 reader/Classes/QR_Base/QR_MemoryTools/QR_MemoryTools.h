/*****************************************************************************
 * ==> QR_MemoryTools -------------------------------------------------------*
 * ***************************************************************************
 * Description : Some memory tools                                           *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#ifndef QR_MemoryToolsH
#define QR_MemoryToolsH

// qr engine
#include "QR_Types.h"

//------------------------------------------------------------------------------
// Macros
//------------------------------------------------------------------------------
#define M_ArraySize(array) (sizeof(array) / sizeof((array)[0]))
//------------------------------------------------------------------------------

/**
* Some memory tools
*@author Jean-Milost Reymond
*/
class QR_MemoryTools
{
    public:
        /**
        * Buffer comparison type
        */
        enum IEBufferCompType
        {
            IE_BC_FromStartToEnd,
            IE_BC_FromEndToStart,
            IE_BC_Symmetric,
        };

        /**
        * Buffer description for comparison functions
        */
        struct IBuffer
        {
            const QR_BufferDataType* m_pBuffer;
                  QR_BufferSizeType  m_Length;

            IBuffer();
            virtual ~IBuffer();
        };

        QR_MemoryTools();
        virtual ~QR_MemoryTools();

        /**
        * Checks if system on which program is executed is big endian
        *@return true if system on which program is executed is big endian, false if little endian
        */
        static inline bool IsSystemBE();

        /**
        * Binary compares buffers content
        *@param buffer1 - first buffer to compare
        *@param buffer2 - second buffer to compare
        *@param type - comparison type
        *@param blockLength - length of memory block to compare on each cycle
        *@return true if buffers are identical, otherwise false
        *@throw Exception on error
        */
        static bool CompareBuffers(const IBuffer&         buffer1,
                                   const IBuffer&         buffer2,
                                         IEBufferCompType type,
                                         QR_SizeT         blockLength);
};

//---------------------------------------------------------------------------
// QR_MemoryTools - c++ cross-platform
//---------------------------------------------------------------------------
bool QR_MemoryTools::IsSystemBE()
{
    union
    {
        QR_UInt32 i;
        QR_Int8   c[4];
    } bInt = {0x01020304};

    return (bInt.c[0] == 1);
}
//---------------------------------------------------------------------------

#endif // QR_MemoryToolsH
