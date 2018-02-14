module Main where

import Prelude
import Control.Monad.Eff

import Math
import Global.Unsafe

import Data.Int
import Data.Array
import Data.Maybe
import Data.Array.ST

import Graphics.Canvas
import Partial.Unsafe (unsafePartial)

import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Console(CONSOLE,log,logShow)
import Control.Monad.Eff.Timer
import Control.Monad.Eff.Ref (REF, readRef, modifyRef, newRef)

import DOM (DOM)
import Control.Monad.Eff.DOM (addEventListener, animate)
import FFI.Util (property, setProperty)

clear ctx = do
  _ <- setFillStyle "#FFFFFF" ctx
  fillRect ctx { x: -300.0, y: -300.0, w: 640.0, h: 640.0 }

draw startX startY endX endY ctx = strokePath ctx $ do
	_ <- setStrokeStyle "#0000FF" ctx
	_ <- moveTo ctx startX startY
	_ <- lineTo ctx endX endY
	_ <- closePath ctx
	tem <- emptySTArray
	void $ pushSTArray tem 1

rotateX :: Number -> Number -> Number -> Number -> Array Number
rotateX x y z angle = do
	let angleInRad = (angle*pi)/180.0
	let coss = cos angleInRad
	let sins = sin angleInRad

	let changeY = y*coss - z*sins
	let changeZ = z*coss + y*sins

	[x,changeY,changeZ]

rotateY :: Number -> Number -> Number -> Number -> Array Number
rotateY x y z angle = do
	let angleInRad = (angle*pi)/180.0
	let coss = cos angleInRad
	let sins = sin angleInRad

	let changeX = x*coss + z*sins
	let changeZ = z*coss - x*sins

	[changeX,y,changeZ]

{-
rotateZ :: Number -> Number -> Number -> Number -> Array Number
rotateZ x y z angle = do
	let angleInRad = (angle*pi)/180.0
	let coss = cos angleInRad
	let sins = sin angleInRad

	let changeX = x*coss - y*sins
	let changeY = y*coss + x*sins

	[changeX,changeY,z]
-}

cube nodes edges ctx = void $ forE 0 12 $ \i -> do

	edgeTemp <- peekSTArray edges i
	let edge = fromMaybe [] edgeTemp
	let edge0Temp = edge !! 0
	let edge1Temp = edge !! 1

	let edge0 = fromMaybe 0 edge0Temp
	let edge1 = fromMaybe 0 edge1Temp

	sxTemp <- peekSTArray nodes edge0
	syTemp <- peekSTArray nodes edge1

	let sxMemp = fromMaybe [] sxTemp
	let syMemp = fromMaybe [] syTemp

	let sxDemp = sxMemp !! 0
	let syDemp = sxMemp !! 1

	let exDemp = syMemp !! 0
	let eyDemp = syMemp !! 1

	let sx = fromMaybe 0.0 sxDemp
	let sy = fromMaybe 0.0 syDemp

	let ex = fromMaybe 0.0 exDemp
	let ey = fromMaybe 0.0 eyDemp

	draw sx sy ex ey ctx

drag :: Array Int
drag = [0]

prevX :: Array Number
prevX = [0.0]

prevY :: Array Number
prevY = [0.0]

diffX :: Array Number
diffX = [0.0]

diffY :: Array Number
diffY = [0.0]

acl :: Array Number
acl = [0.0]

roX :: Array Number
roX = [0.0]

roY :: Array Number
roY = [0.0]

initialNodes = [
	[-100.0,-100.0,-100.0],
	[-100.0,-100.0, 100.0],
	[-100.0, 100.0,-100.0],
	[-100.0, 100.0, 100.0],
	[ 100.0,-100.0,-100.0],
	[ 100.0,-100.0, 100.0],
	[ 100.0, 100.0,-100.0],
	[ 100.0, 100.0, 100.0]
]

initialEdges = [
	[0, 1],
	[1, 3],
	[3, 2],
	[2, 0],
	[4, 5],
	[5, 7],
	[7, 6],
	[6, 4],
	[0, 4],
	[1, 5],
	[2, 6],
	[3, 7]
]

updateCube xang yang ctx = do
	nodes <- emptySTArray
	void $ forE 0 8 $ \i -> do
		let temp = (initialNodes !! i)
		let temp2 = fromMaybe [] temp
		void $ pushSTArray nodes temp2

	edges <- emptySTArray
	void $ forE 0 12 $ \i -> do
		let temp = (initialEdges !! i)
		let temp2 = fromMaybe [] temp
		void $ pushSTArray edges temp2

	
	rotated <- emptySTArray
	void $ forE 0 8 $ \i -> do
		nodesEx <- peekSTArray nodes i
		let cd = fromMaybe [] nodesEx
		let fx = cd !! 0
		let fy = cd !! 1
		let fz = cd !! 2

		let x = fromMaybe 0.0 fx
		let y = fromMaybe 0.0 fy
		let z = fromMaybe 0.0 fz

		let first = rotateX x y z (xang)
		let mx = first !! 0
		let my = first !! 1
		let mz = first !! 2

		let mxx = fromMaybe 0.0 mx
		let myy = fromMaybe 0.0 my
		let mzz = fromMaybe 0.0 mz

		let second = rotateY mxx myy mzz (yang)
		void $ pushSTArray rotated second
		tem <- emptySTArray
		void $ pushSTArray tem 1

	cube rotated edges ctx
	pure unit

mousedown val = void $ do
	_ <- pure $ setProperty drag "0" 1
	pure unit

mouseup val = void $ do
	_ <- pure $ setProperty drag "0" 0
	pure unit

mousemove val = void $ do
	let cX = property val "clientX"
	let cY = property val "clientY"

	_ <- if (property drag "0") then do
		let (pmX :: Number) = property prevX "0"
		let (pmY :: Number) = property prevY "0"

		let (xy :: Number) = (cX - pmX)
		let (yx :: Number) = (cY - pmY)
		_ <- pure $ setProperty diffX "0" xy
		_ <- pure $ setProperty diffY "0" yx

		let acc = (property acl "0" ) + 2.0
		_ <- pure $ setProperty acl "0" acc
		pure unit
		else do
			pure unit

	_ <- pure $ setProperty prevX "0" cX
	_ <- pure $ setProperty prevY "0" cY
	pure unit


main = void $ unsafePartial do
	Just canvas <- getCanvasElementById "canvas"
	ctx <- getContext2D canvas

	_ <- translate { translateX: 300.0, translateY: 300.0 } ctx

	--updateCube 60.0 60.0 ctx
	_ <- clear ctx
	
	addEventListener canvas "mousedown" mousedown
	addEventListener canvas "mousemove" mousemove
	addEventListener canvas "mouseup" mouseup
	animate ctx finalUpdate
	pure unit

finalUpdate ctx = void $ do
	let acc = (property acl "0")
	_ <- if (acc > 0.0) then do
		_ <- pure $ setProperty acl "0" (acc - 0.5)
		let (dx :: Number)  = property diffX "0"
		let (dy :: Number)  = property diffY "0"
		let (rxx :: Number) = property roX "0"
		let (ryy :: Number) = property roY "0"
		_ <- pure $ setProperty roX "0" (rxx - dx)
		_ <- pure $ setProperty roY "0" (ryy - dy)
		pure unit
		else
			pure unit


	let (rx :: Number) = property roX "0"
	let (ry :: Number) = property roY "0"
	
	_ <- clear ctx
	_ <- updateCube ry rx ctx
	pure unit
