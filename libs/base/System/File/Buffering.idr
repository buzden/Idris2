module System.File.Buffering

import public System.File.Types

%foreign "C:setvbuf,libc 6"
prim__setvbuf : FilePtr -> Ptr Char -> (mode : Int) -> (size : Int) -> PrimIO Int

prim__IOFBF, prim__IOLBF, prim__IONBF : Int
prim__IOFBF = 0 -- Fully buffered
prim__IOLBF = 1 -- Line buffered
prim__IONBF = 2 -- No buffering

public export
data BufferingMode = FullyBuffered | LineBuffered | NoBuffering

export
setBufferingMode : HasIO io => BufferingMode -> File -> io ()
setBufferingMode mode $ FHandle f = do
  let mode = case mode of
               FullyBuffered => prim__IOFBF
               LineBuffered  => prim__IOLBF
               NoBuffering   => prim__IONBF
  ignore $ primIO $ prim__setvbuf f (prim__castPtr prim__getNullAnyPtr) mode 0
