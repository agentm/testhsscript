{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import GHC
import GHC.Paths (libdir)
import Unsafe.Coerce
import Control.Exception
import DynFlags
import GHC.LanguageExtensions
import Type hiding (pprTyThing)
import Outputable hiding ((<>))
import PprTyThing
import System.FilePath.Glob (glob)

import Data.Dynamic

import Control.Monad.IO.Class

import Test (Spam(Spam), SpamFunction)

--I am trying to load testhss
initSession :: IO HscEnv
initSession = runGhc (Just libdir) $ do
  liftIO $ putStrLn "Setting up HscEnv"
  sandboxPkgPaths <- liftIO (glob ".cabal-sandbox/*packages.conf.d")  
  dflags <- getSessionDynFlags
  let dflags' = dflags { hscTarget = HscInterpreted , 
                         ghcLink = LinkInMemory, 
                         {-
                         safeHaskell = Sf_Safe,
                         safeInfer = True,
                         safeInferred = True,
                         -}
                         debugLevel = 1,
                         verbosity = 3,
                         packageFlags = (packageFlags dflags) ++ map (\n -> ExposePackage n (PackageArg n) (ModRenaming True [])) ["Glob", "base", "directory", "dlist", "deepseq", "ghc-prim", "transformers"],
                         extraPkgConfs = const (GlobalPkgConf : localPkgPaths)
                       }
                `xopt_set` ExtendedDefaultRules
                `xopt_set` ImplicitPrelude
                --`gopt_set` Opt_DistrustAllPackages 
                --`gopt_set` Opt_PackageTrust
                `gopt_set` Opt_ImplicitImportQualified
      localPkgPaths = map PkgConfFile sandboxPkgPaths
                
  _ <- setSessionDynFlags dflags'
  liftIO $ putStrLn "past setSessionDynFlags"
  --baseTarget <- guessTarget "Test2" Nothing
  --addTarget baseTarget
  --flag <- load LoadAllTargets
  let flag = Succeeded
  case flag of
    Failed -> error "failed to load"
    Succeeded -> do
      setContext [ IIDecl $ simpleImportDecl (mkModuleName "Prelude"),
                   IIDecl $ simpleImportDecl (mkModuleName "Test"),
                   IIDecl $ simpleImportDecl (mkModuleName "Data.Typeable") ]
      env <- getSession
      pure env
  
--Nothing is success    
typeCheckFunction :: Type -> String -> Ghc ()    
typeCheckFunction expectedType inp = do
  dflags <- getSessionDynFlags  
  let fprint = typeRepFingerprint (typeOf (Spam 5))
  liftIO $ putStrLn ("typecheck " ++ inp ++ " fingerprint " ++ show fprint)
  funcType <- GHC.exprType inp
  --env <- getSession

  let showType ty = showSDocForUser dflags alwaysQualify (pprTypeForUser ty)

  liftIO $ putStrLn ("types: " ++ showType expectedType ++ " / " ++ showType funcType)
  if not (eqType funcType expectedType) then
    error ("type mismatch " ++ showType expectedType ++ " / " ++ showType funcType)
    else
    pure ()
    
compile :: String -> Ghc (Spam)
compile inp = do
  liftIO $ putStrLn ("compile " ++ inp)
  dyn <- fromDynamic <$> dynCompileExpr inp  
  case dyn of 
    Nothing -> do
      io <- compileExpr ("Prelude.print (" ++ inp ++ ")")
      _ <- liftIO (unsafeCoerce io)
      val2 <- compileExpr inp
      let x = unsafeCoerce val2 :: Spam
      liftIO $ putStrLn (show x)
      error "dynCompileExpr failed"
    Just val -> pure val

mkExpectedType2 :: Ghc Type    
mkExpectedType2 = do
  let tc = "Spam (10::Int)"
  liftIO $ putStrLn ("exprType " ++ tc)
  GHC.exprType tc

main :: IO ()
main = do
  env <- initSession
  --let safeImport mod = IIDecl $ (simpleImportDecl (mkModuleName mod)) { ideclSafe = True }
  --let script = "error (show (typeRepFingerprint (typeOf (Spam 5)))) :: Spam"
  let script = "globbo"
  handle (\(exc :: SomeException) -> putStrLn "gotcha") $ runGhc (Just libdir) $ do
    setSession env
    --ctx <- getContext
    expType <- mkExpectedType2
    typeCheckFunction expType script
    liftIO $ putStrLn "before compile"
    val <- compile script
    liftIO $ putStrLn "after compile"
    --let x = val (Spamv 4)
    liftIO $ putStrLn (show val)
    

