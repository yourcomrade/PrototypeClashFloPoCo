{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Help (
   infoEn,
   
    
) where
import Tem 

infoEn = $(genFloPoCoInfoEntity floPoCoPath args filePath)
