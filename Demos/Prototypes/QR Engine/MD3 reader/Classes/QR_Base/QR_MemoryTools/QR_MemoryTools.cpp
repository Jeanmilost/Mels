/*****************************************************************************
 * ==> QR_MemoryTools -------------------------------------------------------*
 * ***************************************************************************
 * Description : Some memory tools                                           *
 * Developer   : Jean-Milost Reymond                                         *
 *****************************************************************************/

#include "QR_MemoryTools.h"

// std
#include <mem.h>

// qr engine
#include "QR_Exception.h"

//---------------------------------------------------------------------------
// QR_MemoryTools::IBuffer - c++ cross-platform
//---------------------------------------------------------------------------
QR_MemoryTools::IBuffer::IBuffer() : m_pBuffer(NULL), m_Length(0)
{}
//---------------------------------------------------------------------------
QR_MemoryTools::IBuffer::~IBuffer()
{}
//---------------------------------------------------------------------------
// QR_MemoryTools - c++ cross-platform
//---------------------------------------------------------------------------
QR_MemoryTools::QR_MemoryTools()
{}
//---------------------------------------------------------------------------
QR_MemoryTools::~QR_MemoryTools()
{}
//---------------------------------------------------------------------------
bool QR_MemoryTools::CompareBuffers(const IBuffer&         buffer1,
                                    const IBuffer&         buffer2,
                                          IEBufferCompType type,
                                          QR_SizeT         blockLength)
{
    // are buffers defined?
    if (!buffer1.m_pBuffer || !buffer2.m_pBuffer)
        // return true only if both buffers are NULL
        return (!buffer1.m_pBuffer && !buffer2.m_pBuffer);

    // are lengths equals?
    if (buffer1.m_Length != buffer2.m_Length)
        return false;

    // calculate memory block count to compare
    const QR_SizeT count = (buffer1.m_Length / blockLength) + (buffer1.m_Length % blockLength ? 1 : 0);

    // iterate through memory blocks to compare
    for (QR_SizeT i = 0; i < count; ++i)
        switch (type)
        {
            case IE_BC_FromStartToEnd:
            {
                // calculate next offset
                const QR_BufferSizeType offset = i * blockLength;

                // get next block length to compare
                const QR_SizeT length = std::min(blockLength, buffer1.m_Length - offset);

                // is memory block identical?
                #if defined(__CODEGEARC__) || defined(__APPLE__)
                    if (std::memcmp(buffer1.m_pBuffer + offset, buffer2.m_pBuffer + offset, length) != 0)
                #else
                    if (memcmp(buffer1.m_pBuffer + offset, buffer2.m_pBuffer + offset, length) != 0)
                #endif // __CODEGEARC__ / __APPLE__
                        return false;

                continue;
            }

            case IE_BC_FromEndToStart:
            {
                // calculate next offset
                const QR_BufferSizeType offset = ((count - i) - 1) * blockLength;

                // get next block length to compare
                const QR_SizeT length = std::min(blockLength, buffer1.m_Length - offset);

                // is memory block identical?
                #if defined(__CODEGEARC__) || defined(__APPLE__)
                    if (std::memcmp(buffer1.m_pBuffer + offset, buffer2.m_pBuffer + offset, length) != 0)
                #else
                    if (memcmp(buffer1.m_pBuffer + offset, buffer2.m_pBuffer + offset, length) != 0)
                #endif // __CODEGEARC__ / __APPLE__
                        return false;

                continue;
            }

            case IE_BC_Symmetric:
            {
                // calculate next start and end offsets
                const QR_BufferSizeType startOffset = i * blockLength;
                const QR_BufferSizeType endOffset   = ((count - i) - 1) * blockLength;

                // middle of the buffer is reached?
                if (startOffset >= endOffset && buffer1.m_Length > blockLength)
                    return true;

                // get next start and end block lengths to compare
                const QR_SizeT startLength = std::min(blockLength, buffer1.m_Length - startOffset);
                const QR_SizeT endLength   = std::min(blockLength, buffer1.m_Length - endOffset);

                // is start memory block identical?
                #if defined(__CODEGEARC__) || defined(__APPLE__)
                    if (std::memcmp(buffer1.m_pBuffer + startOffset, buffer2.m_pBuffer + startOffset, startLength) != 0)
                #else
                    if (memcmp(buffer1.m_pBuffer + startOffset, buffer2.m_pBuffer + startOffset, startLength) != 0)
                #endif // __CODEGEARC__ / __APPLE__
                        return false;

                // is end memory block identical?
                #if defined(__CODEGEARC__) || defined(__APPLE__)
                    if (std::memcmp(buffer1.m_pBuffer + endOffset, buffer2.m_pBuffer + endOffset, endLength) != 0)
                #else
                    if (memcmp(buffer1.m_pBuffer + endOffset, buffer2.m_pBuffer + endOffset, endLength) != 0)
                #endif // __CODEGEARC__ / __APPLE__
                        return false;

                continue;
            }

            default:
                M_THROW_EXCEPTION("Unknown comparison type");
        }

    return true;
}
//---------------------------------------------------------------------------

