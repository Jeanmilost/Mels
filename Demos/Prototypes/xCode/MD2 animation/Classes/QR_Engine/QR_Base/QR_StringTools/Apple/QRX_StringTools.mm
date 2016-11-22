/*****************************************************************************
 * ==> QRX_StringTools ------------------------------------------------------*
 * ***************************************************************************
 * Description : Some tools to convert std::string <==> NSString             *
 * Developper  : Jean-Milost Reymond                                         *
 *****************************************************************************/

#import "QRX_StringTools.h"

#ifdef TARGET_RT_BIG_ENDIAN
    const NSStringEncoding kEncoding_wchar_t =
            CFStringConvertEncodingToNSStringEncoding(kCFStringEncodingUTF32LE);
#else
    const NSStringEncoding kEncoding_wchar_t =
            CFStringConvertEncodingToNSStringEncoding(kCFStringEncodingUTF32BE);
#endif // TARGET_RT_BIG_ENDIAN

//------------------------------------------------------------------------------
// QRX_StringTools - objective c Apple specific
//------------------------------------------------------------------------------
@implementation QRX_StringTools
//------------------------------------------------------------------------------
+ (std::string)NSStrToStr :(NSString*)pStr
{
    NSData* asData = [pStr dataUsingEncoding:CFStringConvertEncodingToNSStringEncoding(kCFStringEncodingUTF8)];
    return std::string((char*)[asData bytes], [asData length]);
}
//------------------------------------------------------------------------------
+ (std::wstring)NSStrToWStr :(NSString*)pStr
{
    NSData* asData = [pStr dataUsingEncoding:kEncoding_wchar_t];
    return std::wstring((wchar_t*)[asData bytes], [asData length] / sizeof(wchar_t));
}
//------------------------------------------------------------------------------
+ (NSString*)StrToNSStr :(const std::string&)str
{
    return [[[NSString alloc]initWithUTF8String :str.c_str()]autorelease];
}
//------------------------------------------------------------------------------
+ (NSString*)WStrToNSStr :(const std::wstring&)wStr
{
    char*       pData = (char*)wStr.data();
    std::size_t size  = wStr.size() * sizeof(wchar_t);

    return [[[NSString alloc]initWithBytes:pData length:size encoding:kEncoding_wchar_t]autorelease];
}
//------------------------------------------------------------------------------
@end
//------------------------------------------------------------------------------
