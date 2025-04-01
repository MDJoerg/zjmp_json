*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
* Constants for kernel call operation codes
DEFINE include_constants.
  data:
   begin of ssf_opcodes,
     encode                  type x value  1,
     decode                  type x value  2,
     sign                    type x value  3,
     verify                  type x value  4,
     envelope                type x value  5,
     develope                type x value  6,
     digest                  type x value  7,
     addsign                 type x value  8,
     version                 type x value  9,
     pwencrypt               type x value 10,
     psemanagement           type x value 11,
     queryproperties         type x value 12,
     ssftoolkitlist          type x value 13,

     ssfv2_parsecert         type x value 20,
     ssfv2_getcert           type x value 21,
     ssfv2_putcert           type x value 22,
     ssfv2_removecert        type x value 23,
     ssfv2_getcertlist       type x value 24,
     ssfv2_getowncert        type x value 25,
     ssfv2_creatprofile      type x value 26,
     ssfv2_getcertificaterequest  type x value 27,
     ssfv2_putcertificateresponse type x value 28,
     ssfv2_getsapcacertificate    type x value 29, "not yet available

     base64code              type x value 31,
     parseobject             type x value 32,
     psechangepin            type x value 33,
     psecreatecred           type x value 34,
     certlist_pkcs7wrap      type x value 35,
     base64encode            type x value 36,
     base64decode            type x value 37,

     sign_ex                 type x value 40,
     verify_ex               type x value 41,

     canonize_id             type x value 51,
   end of ssf_opcodes.
END-OF-DEFINITION.
