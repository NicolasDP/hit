-- |
-- Module      : Data.Git.Diff
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unix
--

module Data.Git.Diff
    ( HitDiff
    , getDiff
    ) where

import Control.Applicative ((<$>))
import Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Char8 as BS

import Data.Git
import Data.Git.Repository
import Data.Git.Storage
import Data.Git.Storage.Object

import Data.Algorithm.Diff (getGroupedDiff, Diff(..))

-- | This represents a diff.
-- (filename,right,Index1,Index2,[Diff [t]])
-- where filename is the name of the file
--       right comes from HTree
--       index1 is a reference to this file's object
--       index2 is an other reference to this file's object
--       a list of grouped diff (see Data.Algorithm.Diff)
type HitDiff      = (BS.ByteString,Int,Ref,Ref,[Diff [L.ByteString]])
type HitDiffChunk = (BS.ByteString,Int,Ref    ,      [L.ByteString] )

buildListForDiff :: Git -> Revision -> IO [HitDiffChunk]
buildListForDiff git revision = do
    ref    <- maybe (error "revision cannot be found") id <$> resolveRevision git revision
    commit <- getCommit git ref
    tree   <- resolveTreeish git $ commitTreeish commit
    case tree of
        Just t -> do htree <- buildHTree git t
                     buildTreeList htree [] (BS.empty)
        _      -> error "cannot build a tree from this reference"
    where
        buildTreeList :: HTree -> [HitDiffChunk] -> BS.ByteString -> IO [HitDiffChunk]
        buildTreeList []                     list _          = return list
        buildTreeList ((d,n,TreeFile r):xs)  list pathPrefix = do
            content <- catBlobFile r
            buildTreeList xs ((BS.append pathPrefix n,d,r, L.lines content) : list) pathPrefix
        buildTreeList ((_,n,TreeDir r _):xs) list pathPrefix = do
            subTree <- resolveTreeish git r
            case subTree of
                Just subT -> do subHTree <- buildHTree git subT
                                l1 <- buildTreeList xs list pathPrefix
                                l2 <- buildTreeList subHTree [] (BS.concat [pathPrefix, n, BS.pack "/"])
                                return $ l1 ++ l2
                Nothing   -> error "cannot build a subtree from this reference"

        catBlobFile :: Ref -> IO L.ByteString
        catBlobFile ref = do
            mobj <- getObjectRaw git ref True
            case mobj of
                Nothing  -> error "not a valid object"
                Just obj -> return $ oiData obj

-- | It returns a diff between two revisions
getDiff :: Revision -- ^ commit revision
        -> Revision -- ^ commit revision
        -> Git      -- ^ repository
        -> IO [HitDiff]
getDiff rev1 rev2 git = do
    commit1 <- buildListForDiff git rev1
    commit2 <- buildListForDiff git rev2
    return $ purgeDiff $ buildDiff commit1 commit2
    where
        purgeDiff :: [HitDiff] -> [HitDiff]
        purgeDiff []                       = []
        purgeDiff ((file,i,t1,t2,diff):xs) =
            if (onlyBoth diff) then purgeDiff xs
                               else (file,i,t1,t2,diff) : (purgeDiff xs)

        onlyBoth :: [Diff [L.ByteString]] -> Bool
        onlyBoth []            = True
        onlyBoth (Both _ _:xs) = onlyBoth xs
        onlyBoth (_       :_) = False

        buildDiff :: [HitDiffChunk] -> [HitDiffChunk] -> [HitDiff]
        buildDiff []                  []                  = []
        buildDiff ((n1,i1,r1,a1):xs1) []                  = (n1,i1,r1,r1,getGroupedDiff a1 []) : (buildDiff xs1 [])
        buildDiff []                  ((n2,i2,r2,a2):xs2) = (n2,i2,r2,r2,getGroupedDiff [] a2) : (buildDiff []  xs2)
        buildDiff ((n1,i1,r1,a1):xs1) ((n2,i2,r2,a2):xs2) =
            if n1 == n2 then (n1,i1,r1,r2,getGroupedDiff a1 a2) : (buildDiff xs1 xs2)
                        else (buildDiff [(n1,i1,r1,a1)] xs2) ++ (buildDiff xs1 ((n2,i2,r2,a2):xs2))
