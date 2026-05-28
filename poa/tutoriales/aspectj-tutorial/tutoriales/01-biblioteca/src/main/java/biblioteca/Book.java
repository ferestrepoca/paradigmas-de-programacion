package biblioteca;

/** Entidad de dominio — representa un libro del catálogo. */
public class Book {

    private final String isbn;
    private final String title;
    private boolean available;

    public Book(String isbn, String title) {
        this.isbn = isbn;
        this.title = title;
        this.available = true;
    }

    public String getIsbn()   { return isbn; }
    public String getTitle()  { return title; }
    public boolean isAvailable() { return available; }
    public void setAvailable(boolean available) { this.available = available; }

    @Override
    public String toString() {
        return "\"" + title + "\" [" + isbn + "]";
    }
}
