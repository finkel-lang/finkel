;;; -*- mode: finkel -*-
;;; Loop for network connection.

(defmodule Finkel.Tool.Internal.Listen
  (export listener accept-loop run-conn)
  (require
   ;; finkel-core
   (Finkel.Tool.Internal.Macro.Ghc))
  (import
   ;; base
   (Control.Concurrent [(MVar) forkIO newEmptyMVar putMVar takeMVar])
   (Control.Exception [(Exception ..) (SomeException ..) handle])
   (Control.Monad [void unless when])
   (Data.Char [isSpace])
   (System.IO
    [(BufferMode ..) (IOMode ..) hClose hFlush hPutStr hSetBuffering
     hSetEncoding utf8])

   ;; bytestring
   (Data.ByteString.Internal [toForeignPtr])
   (qualified Data.ByteString.Char8 as BS)

   ;; network
   (Network.Socket
    [(AddrInfo ..) (AddrInfoFlag ..) PortNumber Socket (SocketOption ..)
     (SocketType ..) accept bind defaultHints getAddrInfo listen socket
     socketToHandle setSocketOption withSocketsDo])

   ;; finkel-kernel
   (Language.Finkel.Lexer [evalSP])
   (Language.Finkel.Reader [sexpr])

   ;; internal
   (Finkel.Tool.Internal.Types)))

(imports-from-ghc
 (GHC.Data.StringBuffer [(StringBuffer ..)]))

(defn (:: listener (-> PortNumber (MVar Input) (IO ())))
  [pnum mvar]
  (withSocketsDo
   (lefn [(hints (defaultHints {(= addrFlags [AI_PASSIVE])
                                (= addrSocketType Stream)}))
          (start-loop [addr]
            (do (<- sock (socket (addrFamily addr)
                                 (addrSocketType addr)
                                 (addrProtocol addr)))
                (setSocketOption sock ReuseAddr 1)
                (bind sock (addrAddress addr))
                (listen sock 2)
                (accept-loop sock mvar)))]
     (case-do (getAddrInfo (Just hints) Nothing (Just (show pnum)))
       (: addr _) (start-loop addr)
       _          (putStrLn "listener: unable to open address.")))))

(defn (:: accept-loop (-> Socket (MVar Input) (IO ())))
  [sock mvar]
  (do (<- (, conn _) (accept sock))
      (<- _ (forkIO (run-conn conn mvar)))
      (accept-loop sock mvar)))

(defn (:: run-conn (-> Socket (MVar Input) (IO ())))
  [sock mvar]
  (do (<- hdl (socketToHandle sock ReadWriteMode))
      (hSetBuffering hdl (BlockBuffering Nothing))
      (hSetEncoding hdl utf8)
      (hPutStr hdl "Connected to Finkel REPL.")
      (hFlush hdl)
      (<- my-mvar newEmptyMVar)
      (lefn [(handler [(SomeException e)]
               (do (putStrLn (++ "run-conn: " (show e)))
                   (hClose hdl)))
             (put-input [form]
               (putMVar mvar (Input Connection form my-mvar)))
             (read-loop
               ;; `BS.hGetSome' returns empty contents when the handle is closed.
               ;; Also, empty lines could be sent as input, evaluating the form
               ;; only when it contained non-space characters.
               (do (<- bs (BS.hGetSome hdl 65535))
                   (unless (BS.null bs)
                     (do (when (BS.any (. not isSpace) bs)
                           (lept [(, fp o l) (toForeignPtr bs)
                                  sbuf (StringBuffer fp l o)]
                             (case (evalSP sexpr (Just "<interactive>") sbuf)
                               (Right form) (put-input form)
                               (Left err) (putStrLn (displayException err)))))
                         read-loop))))
             (print-loop
               (do (<- result (takeMVar my-mvar))
                   (case result
                     (Right r) (hPutStr hdl r)
                     (Left err) (hPutStr hdl err))
                   (hFlush hdl)
                   print-loop))])
      (void (forkIO print-loop))
      (handle handler read-loop)))
