--Pour tester, il faut lance le Spec.hs dans le src (copier, coller)
import RLE
import Statistic.EncodingTree
import Statistic.Bit
import qualified LZ.Dictionaries as Dict

buildEncodingTreeExample :: String -> Maybe (EncodingTree Char)
buildEncodingTreeExample _ = Just $
    EncodingNode 5
        (EncodingNode 4
            (EncodingLeaf 2 'a')
            (EncodingLeaf 1 'b'))
        (EncodingLeaf 3 'c')

main :: IO ()
main = do
    --Encoding Tree
    putStrLn $ "Encoding Tree: "
    let text = "abbca"
        (encodingTreeResult, compressedText) = Statistic.EncodingTree.compress buildEncodingTreeExample text
        uncompressedText = Statistic.EncodingTree.uncompress (encodingTreeResult, compressedText)
        longueur_moyenne = meanLength <$> encodingTreeResult
    putStrLn $ "Texte initial : " ++ show text
    putStrLn $ "Texte compressé : " ++ show compressedText
    putStrLn $ "Texte décompressé : " ++ show uncompressedText
    putStrLn $ "Longueur moyenne : " ++ maybe "Arbre d'encodage non disponible" show longueur_moyenne
    --RLE
    putStrLn $ "RLE : "
    let compressed_text_RLE = RLE.compress text
        uncompressed_text_RLE = RLE.uncompress compressed_text_RLE
    putStrLn $ "Texte initial : " ++ show text
    putStrLn $ "Texte compressé : " ++ show compressed_text_RLE
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_RLE
