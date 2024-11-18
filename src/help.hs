{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Help (
   --infoEn,
   --infoEn2,
   infoEnVGAController,
    
) where
import Tem 
import Lexer
import Prelude
--infoEn = $(genFloPoCoInfoEntity floPoCoPath args filePath)
--infoEn2 = $(genFloPoCoInfoEntity floPoCoPath args2 filePath2)
infoEnVGAController :: InfoEntity
infoEnVGAController = InfoEntity {
    name     = Just "vga_controller",
    freq     = Just 100,
    pipedep  = Nothing,
    insig    = Just ["clk_100MHz", "reset"],
    outsig   = Just ["video_on", "hsync", "vsync", "p_tick", "x", "y"]
}
