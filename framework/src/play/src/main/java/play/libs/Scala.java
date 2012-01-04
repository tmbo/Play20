package play.libs;

import java.util.*;

/*
 * class that contains useful java <-> scala conversion helpers
 */
public class Scala {

    public static <T> T orNull(scala.Option<T> opt) {
        if(opt.isDefined()) {
            return opt.get();
        }
        return null;
    }

    public static <K,V> java.util.Map<K,V> asJava(scala.collection.Map<K,V> scalaMap) {
       return scala.collection.JavaConverters.asJavaMapConverter(scalaMap).asJava();
    }

    public static <T> java.util.List<T> asJava(scala.collection.immutable.List<T> scalaList) {
       return scala.collection.JavaConverters.asJavaListConverter(scalaList).asJava();
    }

    public static <T> scala.collection.Seq<T> toSeq(java.util.List<T> list) {
        return scala.collection.JavaConverters.asScalaBufferConverter(list).asScala().toList();
    }

    public static <T> scala.collection.Seq<T> toSeq(T[] array) {
        return toSeq(java.util.Arrays.asList(array));
    }
    
    public static <T> scala.collection.Seq<T> varargs(T... array) {
        return toSeq(java.util.Arrays.asList(array));
    }

    public static <T> scala.Option<T> Option(T t) {
        return scala.Option.apply(t);
    }
    
    public static <A,B> scala.Tuple2<A,B> Tuple(A a, B b) {
        return new scala.Tuple2(a, b);
    }
    
    public static <T> scala.collection.Seq<T> emptySeq() {
        return (scala.collection.Seq<T>)toSeq(new Object[] {});
    }
    
    public static <A,B> scala.collection.immutable.Map<A,B> emptyMap() {
        return new scala.collection.immutable.HashMap<A,B>();
    }

    //

    public static abstract class Function0<T> implements scala.Function0<T> {

        @Override
        public double apply$mcD$sp() {
            return scala.Function0$class.apply$mcD$sp(this);
        }

        @Override
        public float apply$mcF$sp() {
            return scala.Function0$class.apply$mcF$sp(this);
        }

        @Override
        public long apply$mcJ$sp() {
            return scala.Function0$class.apply$mcJ$sp(this);
        }

        @Override
        public int apply$mcI$sp() {
            return scala.Function0$class.apply$mcI$sp(this);
        }

        @Override
        public char apply$mcC$sp() {
            return scala.Function0$class.apply$mcC$sp(this);
        }

        @Override
        public short apply$mcS$sp() {
            return scala.Function0$class.apply$mcS$sp(this);
        }

        @Override
        public byte apply$mcB$sp() {
            return scala.Function0$class.apply$mcB$sp(this);
        }

        @Override
        public boolean apply$mcZ$sp() {
            return scala.Function0$class.apply$mcZ$sp(this);
        }

        @Override
        public void apply$mcV$sp() {
            scala.Function0$class.apply$mcV$sp(this);
        }


    }

    public static abstract class Function1<T,U> implements scala.Function1<T,U> {

        @Override
        public <A> scala.Function1<A, U> compose(scala.Function1<A, T> f) {
            return scala.Function1$class.compose(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcVJ$sp(scala.Function1<scala.runtime.BoxedUnit,A> f) {
            return scala.Function1$class.andThen$mcVJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcVI$sp(scala.Function1<scala.runtime.BoxedUnit,A> f) {
            return scala.Function1$class.andThen$mcVI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcVF$sp(scala.Function1<scala.runtime.BoxedUnit,A> f) {
            return scala.Function1$class.andThen$mcVF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcVD$sp(scala.Function1<scala.runtime.BoxedUnit,A> f) {
            return scala.Function1$class.andThen$mcVD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcJJ$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcJJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcIJ$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcIJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcDJ$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcDJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcFJ$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcFJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcZJ$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcZJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcJI$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcJI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcII$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcII$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcFI$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcFI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcDI$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcDI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcZI$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcZI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcJF$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcJF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcIF$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcIF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcFF$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcFF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcDF$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcDF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcZF$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcZF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcJD$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcJD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcID$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcID$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcFD$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcFD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcDD$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcDD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<Object,A> andThen$mcZD$sp(scala.Function1<Object,A> f) {
            return scala.Function1$class.andThen$mcZD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,scala.runtime.BoxedUnit> compose$mcVJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcVJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcJJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcJJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcIJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcIJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcFJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcFJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcDJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcDJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcZJ$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcZJ$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,scala.runtime.BoxedUnit> compose$mcVI$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcVI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcJI$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcJI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcII$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcII$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcFI$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcFI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcDI$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcDI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcZI$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcZI$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,scala.runtime.BoxedUnit> compose$mcVF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcVF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcJF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcJF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcIF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcIF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcFF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcFF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcDF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcDF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcZF$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcZF$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,scala.runtime.BoxedUnit> compose$mcVD$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcVD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcJD$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcJD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcID$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcID$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcFD$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcFD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcDD$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcDD$sp(this, f);
        }

        @Override
        public <A> scala.Function1<A,Object> compose$mcZD$sp(scala.Function1<A,Object> f) {
            return scala.Function1$class.compose$mcZD$sp(this, f);
        }

        @Override
        public void apply$mcVJ$sp(long l) {
            scala.Function1$class.apply$mcVJ$sp(this, l);
        }

        @Override
        public long apply$mcJJ$sp(long l) {
            return scala.Function1$class.apply$mcJJ$sp(this, l);
        }

        @Override
        public int apply$mcIJ$sp(long l) {
            return scala.Function1$class.apply$mcIJ$sp(this, l);
        }

        @Override
        public float apply$mcFJ$sp(long l) {
            return scala.Function1$class.apply$mcFJ$sp(this, l);
        }

        @Override
        public double apply$mcDJ$sp(long l) {
            return scala.Function1$class.apply$mcDJ$sp(this, l);
        }

        @Override
        public boolean apply$mcZJ$sp(long l) {
            return scala.Function1$class.apply$mcZJ$sp(this, l);
        }

        @Override
        public void apply$mcVI$sp(int l) {
            scala.Function1$class.apply$mcVI$sp(this, l);
        }

        @Override
        public long apply$mcJI$sp(int l) {
            return scala.Function1$class.apply$mcJI$sp(this, l);
        }

        @Override
        public int apply$mcII$sp(int l) {
            return scala.Function1$class.apply$mcII$sp(this, l);
        }

        @Override
        public float apply$mcFI$sp(int l) {
            return scala.Function1$class.apply$mcFI$sp(this, l);
        }

        @Override
        public double apply$mcDI$sp(int l) {
            return scala.Function1$class.apply$mcDI$sp(this, l);
        }

        @Override
        public boolean apply$mcZI$sp(int l) {
            return scala.Function1$class.apply$mcZI$sp(this, l);
        }

        @Override
        public void apply$mcVF$sp(float l) {
            scala.Function1$class.apply$mcVF$sp(this, l);
        }

        @Override
        public long apply$mcJF$sp(float l) {
            return scala.Function1$class.apply$mcJF$sp(this, l);
        }

        @Override
        public int apply$mcIF$sp(float l) {
            return scala.Function1$class.apply$mcIF$sp(this, l);
        }

        @Override
        public float apply$mcFF$sp(float l) {
            return scala.Function1$class.apply$mcFF$sp(this, l);
        }

        @Override
        public double apply$mcDF$sp(float l) {
            return scala.Function1$class.apply$mcDF$sp(this, l);
        }

        @Override
        public boolean apply$mcZF$sp(float l) {
            return scala.Function1$class.apply$mcZF$sp(this, l);
        }

        @Override
        public void apply$mcVD$sp(double l) {
            scala.Function1$class.apply$mcVD$sp(this, l);
        }

        @Override
        public long apply$mcJD$sp(double l) {
            return scala.Function1$class.apply$mcJD$sp(this, l);
        }

        @Override
        public int apply$mcID$sp(double l) {
            return scala.Function1$class.apply$mcID$sp(this, l);
        }

        @Override
        public float apply$mcFD$sp(double l) {
            return scala.Function1$class.apply$mcFD$sp(this, l);
        }

        @Override
        public double apply$mcDD$sp(double l) {
            return scala.Function1$class.apply$mcDD$sp(this, l);
        }

        @Override
        public boolean apply$mcZD$sp(double l) {
            return scala.Function1$class.apply$mcZD$sp(this, l);
        }

        @Override
        public <A> scala.Function1<T,A> andThen(scala.Function1<U,A> f) {
            return scala.Function1$class.andThen(this, f);
        }

    }

}
