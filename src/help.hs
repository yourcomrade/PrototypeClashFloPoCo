{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Help (
   infoEn,
   infoEn2,
   
    
) where
import Tem 

infoEn = $(genFloPoCoInfoEntity floPoCoPath args filePath)
infoEn2 = $(genFloPoCoInfoEntity floPoCoPath args2 filePath2)