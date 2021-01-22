generic
    type T is private;
    
package Optional is

    
    type Opt_t(IsEmpty : Boolean := True) is
        record
            case IsEmpty is
                when True => null;
                when False => Value : T;
            end case;
        end record;
    

end Optional;
