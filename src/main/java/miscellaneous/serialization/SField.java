package miscellaneous.serialization;

import java.lang.annotation.*;

@Target(ElementType.FIELD)
@Retention(RetentionPolicy.RUNTIME)
public @interface SField {

  int value();
  
  Class<?> type() default Object.class;
  
  Class<? extends Adapter<?, ?>>[] adapters() default {};
  
  int lengthDescriptorSize() default 4;
}
