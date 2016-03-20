/**
 * Provides different data types used by the Konstructs API. Those
 * classes can be used to store, update and view data that you get
 * from and send to the Konstructs API. All classes are
 * immutable. This means that none of the member fields can be
 * changed. This also means that, as long as all data given to those
 * classes is not mutated (or immutable), then those classes are all
 * safe to send to the API or other plugins (actors), without having
 * to care about concurrency issues. Please see the link below to an
 * excellent introduction to immutability by Oracle themselves.
 *
 * @see <a href="https://docs.oracle.com/javase/tutorial/essential/concurrency/immutable.html">Oracles's guide to immutability</a>
 */
package konstructs.api;
