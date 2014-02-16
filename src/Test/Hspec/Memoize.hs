-- Taken from:
--
-- * https://github.com/DanBurton/io-memoize/
-- * http://hackage.haskell.org/package/io-memoize
--
-- Copyright (c) 2012, Dan Burton
--
-- All rights reserved.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
--
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
--
--     * Neither the name of Dan Burton nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module Test.Hspec.Memoize (ioMemo, newIOMemoizer) where

import Control.Concurrent.MVar
import Data.IORef

-- | Memoize an IO action.
-- The action will be performed
-- the first time that it its value is demanded;
-- all subsequent invocations
-- will simply recall the value acquired
-- from the first call.
-- If the value is never demanded,
-- then the action will never be performed.
--
-- This is basically a safe version of
-- 'System.IO.Unsafe.unsafeInterleaveIO'.
-- This function is also thread-safe:
-- it is guaranteed that the action passed in
-- will be performed exactly 0 or 1 times
-- by this code. Exceptions will be propagated
-- to the caller.
--
-- Example usage:
--
-- >>> getLine' <- ioMemo getLine
--
-- >>> replicateM 3 getLine'
-- Hello
-- ["Hello", "Hello", "Hello"]
ioMemo :: IO a -> IO (IO a)
ioMemo action = do
  memo <- newIOMemoizer
  return (memo action)

newIOMemoizer :: IO (IO a -> IO a)
newIOMemoizer = do
  b <- newMVar True
  r <- newIORef undefined
  return (ioMemoizer b r)

ioMemoizer :: MVar Bool -> IORef a -> IO a -> IO a
ioMemoizer b r action = do
  modifyMVar_ b $ \isEmpty ->
    if isEmpty
      then do v <- action
              writeIORef r v
              return False
      else return False
  readIORef r
