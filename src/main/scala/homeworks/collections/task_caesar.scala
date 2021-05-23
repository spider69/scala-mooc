package homeworks.collections

object task_caesar {

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
  def encrypt(word: String, offset: Int): String =
    word.trim.map(addOffset(_, offset))


  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String =
    cipher.trim.map(addOffset(_, -offset))

  private def addOffset(c: Char, offset: Int): Char = {
    val LeftBound = 'A'
    val RightBound = 'Z' + 1
    val RealOffset = offset % (RightBound - LeftBound)

    val newChar = c + RealOffset
    val result = if (newChar >= RightBound) {
      LeftBound + (newChar - RightBound)
    } else if (newChar < LeftBound) {
      RightBound - (LeftBound - newChar)
    } else {
      newChar
    }
    result.toChar
  }

}
