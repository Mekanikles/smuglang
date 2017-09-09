#pragma once

class BufferedInputStream
{
public:
	BufferedInputStream(std::istream& inStream) 
		: m_inStream(inStream)
	{
	}

	operator bool()
	{
		return (m_end != m_begin) || (bool)m_inStream;
	}

	uint currentColumn() { return m_column; }
	uint currentRow() { return m_row; }

	char get()
	{
		char c;
		get(c);
		return c;
	}

	bool get(char& c)
	{
		bool empty = false;
		if (m_end == m_begin)
			empty = fillBuffer() == 0;

		if (!empty)
		{
			c = m_circBuffer[m_begin++];
			m_begin = m_begin % BUF_SIZE;

			if (c == '\n')
			{
				m_row++;
				m_column = 0;
			}
			else
			{
				m_column++;
			}
			//printLine(string("char: '") + c + "', row: " + std::to_string(m_row) + ", column: " + std::to_string(m_column));

			return true;
		}
		else
		{
			c = EOF;
			return false;
		}
	}

	char lookAhead(uint count = 1)
	{
		assert(count < MAX_LOOKAHEAD);
		uint buffered = bufferedLength();
		if (buffered <= count)
			buffered = fillBuffer();

		if (buffered > count)
			return m_circBuffer[(m_begin + count) % BUF_SIZE];
		else
			return EOF;
	}

	void ignore(uint count = 1)
	{
		char c;
		for (int i = 0; i < count; ++i)
			get(c);
	}

	char peek()
	{
		bool empty = false;
		if (m_end == m_begin)
			empty = fillBuffer() == 0;

		if (!empty)
			return m_circBuffer[m_begin];
		else
			return EOF;
	}
private:
	uint bufferedLength()
	{
		return (m_end + BUF_SIZE - m_begin) % BUF_SIZE;
	}

	uint fillBuffer()
	{
		const uint bufSpace = BUF_SIZE - bufferedLength();
		const uint fill = bufSpace - 1; // Leave one char for m_end
		if (fill)
		{
			uint fill1 = std::min(BUF_SIZE - m_end, fill);
			m_inStream.read(&m_circBuffer[m_end], fill1);
			m_end += m_inStream.gcount();

			m_end = m_end % BUF_SIZE;

			uint fill2 = fill - fill1;
			if (fill2)
			{
				m_inStream.read(&m_circBuffer[m_end], fill2);
				m_end += m_inStream.gcount();
			}
		}

		return bufferedLength();
	}

private:
	static const uint MAX_LOOKAHEAD = 63;
	static const uint BUF_SIZE = MAX_LOOKAHEAD + 1;
	char m_circBuffer[BUF_SIZE];
	uint m_begin = 0;
	uint m_end = 0;
	std::istream& m_inStream;
	uint m_row = 1;
	uint m_column = 1;
};

