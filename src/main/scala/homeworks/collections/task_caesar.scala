package homeworks.collections

object task_caesar {

  /**
   * Full alphabet
   * Elements should be orders by Chat.toInt
   */
  private final val ALPHABET = 'A' to 'Z'
  /**
   * Alphabet capacity
   */
  private final val ALPHABET_LENGTH = ALPHABET.length
  /**
   * Offset from 0 to index of first element from [[ALPHABET]]
   */
  private final val ALPHABET_OFFSET = 'A' - 1

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */
  def encrypt(word: String, offset: Int): String = {
    def getEncryptChar(c: Char, offset: Int): Char = {
      val indexWithoutBaseOffset = c - ALPHABET_OFFSET
      val newIndex = indexWithoutBaseOffset + offset % ALPHABET_LENGTH
      // return to start of alphabet if index greater then alphabet length
      (newIndex % ALPHABET_LENGTH + ALPHABET_OFFSET).toChar
    }
    word.foldLeft(new StringBuilder) {
      case (builder, char) => builder += getEncryptChar(char, offset)
    }.toString()
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    def getDecryptChar(c: Char, offset: Int): Char = {
      val indexWithoutBaseOffset = c - ALPHABET_OFFSET + ALPHABET_LENGTH
      val newIndex = indexWithoutBaseOffset - offset % ALPHABET_LENGTH
      (newIndex % ALPHABET_LENGTH + ALPHABET_OFFSET).toChar
    }
    cipher.foldLeft(new StringBuilder) {
      case (builder, char) => builder += getDecryptChar(char, offset)
    }.toString()
  }
}
