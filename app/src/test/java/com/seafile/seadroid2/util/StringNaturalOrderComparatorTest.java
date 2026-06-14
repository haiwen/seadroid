package com.seafile.seadroid2.util;

import static org.junit.Assert.assertEquals;

import com.seafile.seadroid2.ui.comparator.StringNaturalOrderComparator;

import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;


public class StringNaturalOrderComparatorTest {
    @Test
    public void testGermanSorting() {

        List<String> germanList = Arrays.asList("A1", "a1", "ä1", "Ä1", "A10", "a2", "Ö1", "ö1");
        List<String> expected = Arrays.asList("A1", "A10", "a1", "a2", "Ä1", "Ö1", "ä1", "ö1");

        Collections.sort(germanList, new StringNaturalOrderComparator());

        assertEquals(expected, germanList);
    }

    @Test
    public void testFrenchSorting() {
        List<String> frenchList = Arrays.asList("Éclair", "éclair", "élève", "Élève", "A2", "a1", "a10");
        List<String> expected = Arrays.asList("A2", "a1", "a10", "Éclair", "Élève", "éclair", "élève");

        Collections.sort(frenchList, new StringNaturalOrderComparator());

        assertEquals(expected, frenchList);
    }

    @Test
    public void testGeneralNaturalSorting() {
        List<String> list = Arrays.asList("A1", "A2", "B1", "a1", "a2", "b1", "A10");
        List<String> expected = Arrays.asList("A1", "A2", "A10", "a1", "a2", "B1", "b1");

        Collections.sort(list, new StringNaturalOrderComparator());

        assertEquals(expected, list);
    }
}
