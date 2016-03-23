{- Problem 59: XOR decryption

Each character on a computer is assigned a unique code and the preferred
standard is ASCII (American Standard Code for Information Interchange). For
example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.

A modern encryption method is to take a text file, convert the bytes to ASCII,
then XOR each byte with a given value, taken from a secret key. The advantage
with the XOR function is that using the same encryption key on the cipher text,
restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.

For unbreakable encryption, the key is the same length as the plain text
message, and the key is made up of random bytes. The user would keep the
encrypted message and the encryption key in different locations, and without
both "halves", it is impossible to decrypt the message.

Unfortunately, this method is impractical for most users, so the modified
method is to use a password as a key. If the password is shorter than the
message, which is likely, the key is repeated cyclically throughout the
message. The balance for this method is using a sufficiently long password key
for security, but short enough to be memorable.

Your task has been made easy, as the encryption key consists of three lower
case characters. Using cipher.txt (right click and 'Save Link/Target As...'), a
file containing the encrypted ASCII codes, and the knowledge that the plain
text must contain common English words, decrypt the message and find the sum of
the ASCII values in the original text.

-}

module Problem59 where
import Data.Bits (xor)
import Data.Char (ord, chr)
import System.IO

doit = do
  source <- withFile "p059_cipher.txt" ReadMode hGetLine
  putStrLn $ "Source: " ++ source
  putStrLn $ "Decrypted: " ++ (toPlainText source)
  putStrLn $ "Ascii Sum: " ++ (show $ asciiSum (toPlainText source))
  where
    toPlainText = breakEncryption . readSource
    asciiSum = sum . (map ord)

breakEncryption source = breakEncryption' source keys
  where
    breakEncryption' source (k:ks) =
      if testDecryption $ map chr (decrypt k source)
      then map chr (decrypt k source)
      else breakEncryption' source ks

decrypt _ [] = []
decrypt (k1:k2:k3:[]) (c1:[]) = [c1 `xor` k1]
decrypt (k1:k2:k3:[]) (c1:c2:[]) = [c1 `xor` k1, c2 `xor` k2]
decrypt (k1:k2:k3:[]) (c1:c2:c3:cs) =  (c1 `xor` k1):(c2 `xor` k2):(c3 `xor` k3):(decrypt key cs)
  where
    key = [k1, k2, k3]

testDecryption plainText = foldr (\e b -> elem e ws || b) False commonWords
  where
    ws = words plainText
    commonWords = ["the", "some", "there", "are", "were"]

keys = [map ord k | k<-ks]
  where
    ks = [[a, b, c] | a<-alphas, b<-alphas, c<-alphas]
    alphas = ['a'..'z']

readSource source = map (\t -> read t :: Int) (tokenize source)

tokenize = words . replaceCommas

replaceCommas cs = replaceCommas' cs []
  where
    replaceCommas' [] result = result
    replaceCommas' (c:cs) result =
      if c == ','
      then ' ':(replaceCommas' cs result)
      else c:(replaceCommas' cs result)
