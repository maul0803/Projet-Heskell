import RLE
import Statistic.EncodingTree
import qualified Statistic.Huffman
import qualified Statistic.ShannonFano

-- Fonction pour tester un texte donné
testText :: String -> IO ()
testText text = do
    putStrLn $ "Texte: " ++ text
    -- RLE
    putStrLn $ "RLE : "
    let compressed_text_RLE = RLE.compress text
        uncompressed_text_RLE = RLE.uncompress compressed_text_RLE
    putStrLn $ "Texte initial : " ++ show text
    putStrLn $ "Texte compressé : " ++ show compressed_text_RLE
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_RLE
    -- Huffman
    putStrLn $ "Huffman : "
    let (encoding_tree_Huffman, compressed_text_Huffman) = Statistic.EncodingTree.compress Statistic.Huffman.tree text
        uncompressed_text_Huffman = Statistic.EncodingTree.uncompress (encoding_tree_Huffman, compressed_text_Huffman)
        longueur_moyenne_Huffman = meanLength <$> encoding_tree_Huffman
    putStrLn $ "Arbre d'encodage: " ++ show encoding_tree_Huffman
    putStrLn $ "Longueur moyenne : " ++ maybe "Arbre d'encodage non disponible" show longueur_moyenne_Huffman
    putStrLn $ "Texte compressé : " ++ show compressed_text_Huffman
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_Huffman
    -- Shannon
    putStrLn $ "Shannon : "
    let (encoding_tree_Shannon, compressed_text_Shannon) = Statistic.EncodingTree.compress Statistic.ShannonFano.tree text
        uncompressed_text_Shannon = Statistic.EncodingTree.uncompress (encoding_tree_Shannon, compressed_text_Shannon)
        longueur_moyenne_Shannon = meanLength <$> encoding_tree_Shannon
    putStrLn $ "Arbre d'encodage: " ++ show encoding_tree_Shannon
    putStrLn $ "Longueur moyenne : " ++ maybe "Arbre d'encodage non disponible" show longueur_moyenne_Shannon
    putStrLn $ "Texte compressé : " ++ show compressed_text_Shannon
    putStrLn $ "Texte décompressé : " ++ show uncompressed_text_Shannon
    putStrLn $ "---------------------------------------------------------------------------------------------------"

main :: IO ()
main = do
    -- Testez tous les textes en commentaire
    testText "abbca" -- Shannon et Huffman pareils
    testText "abbcbbb" -- Shannon et Huffman pareils
    testText "zzaaabbbbbbcccccccc" -- Shannon et Huffman différents
    testText "xbbzzaaabbbcccccccc" -- Shannon et Huffman différents
    testText "bzzzaaabbbccccc"  -- Shannon et Huffman pareils
    testText "aaaaa"
