import RLE
import Statistic.EncodingTree
import Statistic.Bit
import qualified LZ.Dictionaries as Dict
import qualified Statistic.Huffman as Statistic.EncodingTree

main :: IO ()
main = do
    let text = "abbca"
    --RLE
    putStrLn $ "RLE : "
    let compressed_text_RLE = RLE.compress text
        uncompressed_text_RLE = RLE.uncompress compressed_text_RLE
    putStrLn $ "Texte initial : " ++ show text
    putStrLn $ "Texte compressé : " ++ show compressed_text_RLE
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_RLE
    --Huffman
    putStrLn $ "Huffman : "
    let (encoding_tree_Huffman, compressed_text_Huffman) = Statistic.EncodingTree.compress Statistic.EncodingTree.tree text
        uncompressed_text_Huffman = Statistic.EncodingTree.uncompress (encoding_tree_Huffman, compressed_text_Huffman)
        longueur_moyenne_Huffman = meanLength <$> encoding_tree_Huffman
    putStrLn $ "Arbre d'encodage: " ++ show encoding_tree_Huffman
    putStrLn $ "Longueur moyenne : " ++ maybe "Arbre d'encodage non disponible" show longueur_moyenne_Huffman
    putStrLn $ "Texte initial : " ++ show text
    putStrLn $ "Texte compressé : " ++ show compressed_text_Huffman
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_Huffman