;;; -*- mode: finkel -*-
;;; Module for make sub command.

(defmodule Finkel.Tool.Command.Make
  (export makeMain)
  (import-when [:compile]
    ;; finkel-core
    (Finkel.Prelude))
  (import
   ;; base
   (System.Console.GetOpt [(OptDescr ..) (ArgDescr ..) (ArgOrder ..) getOpt])
   (System.Environment [getProgName withArgs withProgName])

   ;; finkel-kernel
   (Language.Finkel.Main [defaultMainWith])

   ;; finkel-core
   (Finkel.Core)))

(data MakeOption
  (MakeOption {(:: mo-help Bool)}))

(defn (:: initial-make-option MakeOption)
  (MakeOption {(= mo-help False)}))

(defn (:: make-options [(OptDescr (-> MakeOption MakeOption))])
  [(Option [] ["help"]
           (NoArg (\o (o {(= mo-help True)})))
           "Show this help and exit")])

(defn (:: runMain (-> [String] (IO ())))
  "Main function for compiler with macros from `Finkel.Prelude'."
  [args]
  (macrolet ((preloaded ()
               `[,@(map (\mac (, mac (make-symbol mac)))
                        (exported-macros Finkel.Core))]))
    (do (<- me (fmap (flip ++ " make") getProgName))
        (withArgs args (withProgName me (defaultMainWith (preloaded)))))))

(defn (:: makeMain (-> [String] (IO ())))
  [args]
  (lept [(, o _ _) (getOpt Permute make-options args)
         mo (foldl (flip id) initial-make-option o)]
    (if (mo-help mo)
      (runMain ["--fnk-help"])
      (runMain args))))
